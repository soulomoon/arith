# Chapter2: interpreter and linter differ with class based polymorphic

## Div into type class

In chapter1, we see that `eval` and `lint` share the same structure: performing the operation on the results of sub terms.
And in both cases we need to eval `Mul` and `Div` and `Lit`.
This is a sign of that ad-hoc polymorphism can be perform. Then we can use the type class here.

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

It need type annotation to differ from concrete and symbolic interpretation. But Instead of showing the full type annotation we can use `TypeApplications` extension to simplify it.

```haskell
execEval :: Expr -> IO ()
execEval = print . eval @Int
execLint :: Expr -> IO ()
execLint = print . eval @Symbol
```

## Abstract out the recursive decent with recursion scheme

Recursive function is good feature. But it also suffers from non-termination if not written properly. What if we write none recursive one instead and hoist the recursion to type level.
Well recursion scheme is here to help us.

See [intro into recursion-schemes intro](https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html)

* Algebra: `(x, f, f x -> x)`
* Fix type: `Fix f = f (Fix f)`
* Catamorphism:

  ```haskell
    cata :: Functor f => (f a -> a) -> (Fix f -> a)`
    cata f = unFix >>> fmap (cata f) >>> f
  ```

### Find the Algebra for the Ast

polymorphic over a.
the functor f: `ExprF`

```haskell
data ExprF a = MulF a a | DivF a a | LitF Int deriving (Functor)
```

the algebra For Expression

```haskell
ExprF a -> a
```

### The fixed type for Ast

As recursion in terms, we also need a fixed point to tie the knot in type.

```haskell
type Expr = Fix ExprF
```

Now `Expr` Fixed, we can use bidirectional `PatternSynonyms` to gain back the old constructors

```haskell
pattern Mul a b = Fix (MulF a b)
```

Now we can write the interpreter type class in a none recursion style:

```haskell
class Interpret v where
    eval :: Expr -> v
    eval = cata evalF

    evalF :: Algebra ExprF v 
    evalF (MulF x y) = evalMul x y
    evalF (DivF x y) = evalDiv x y
    evalF (LitF a) = evalLit a

    evalMul :: v -> v -> v
    evalDiv :: v -> v -> v
    evalLit :: Int -> v
```
