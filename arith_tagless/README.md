# Chapter1: Simple abstract and concrete Interpreter

## Simple concrete interpreter

The simplest possible arith interpreter in its simplest form written with pure algebraic data type and simple recursive decent.

```haskell
data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int

eval :: Expr -> Int
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Lit a) = a
```

## Simple abstract interpreter

We now can add an abstract interpreter to the story. It detect naive divide by zero error by performing a symbolic execution, and return `NotKnown` as the final result.

```haskell
data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
lint :: Expr -> Symbol
lint (Mul x y) =
    let xx = lint x
        yy = lint y
    in  if xx == NotKnown || yy == NotKnown
            then NotKnown
            else if xx == Zero || yy == Zero then Zero else NotZero
lint (Div x y) =
    let xx = lint x
        yy = lint y
    in  if yy == Zero || yy == NotKnown then NotKnown else xx
lint (Lit a) = if a == 0 then Zero else NotZero
```

## Class based polymorphic

In above, we see that `eval` and `lint` share the same structure: performing the operation on the results of sub terms.
And in both cases we need to eval `Mul` and `Div` and `Lit`.
This is a sign of that ad-hoc polymorphism can be perform. Then we can use type class.

```haskell
class Interpret v where
    eval :: Expr -> v
    eval (Mul x y) = evalMul (eval x) (eval y)
    eval (Div x y) = evalDiv (eval x) (eval y)
    eval (Lit a) = evalLit a

    evalMul :: v -> v -> v
    evalDiv :: v -> v -> v
    evalLit :: Int -> v

instance Interpret Int where
    evalMul = (*)
    evalDiv = div
    evalLit = id

instance Interpret Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = NotKnown
                  | xx == Zero || yy == Zero         = Zero
                  | otherwise                        = NotZero
    evalDiv xx yy = if yy == Zero || yy == NotKnown then NotKnown else xx
    evalLit a = if a == 0 then Zero else NotZero
```

It need type annotation to differ concrete interpretation from symbolic interpretation. But Instead of showing the full type annotation we can use `TypeApplications` extension to simplify it.

```haskell
execEval :: Expr -> IO ()
execEval = print . eval @Int
execLint :: Expr -> IO ()
execLint = print . eval @Symbol
```
