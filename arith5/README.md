# Chapter 4: Tag the tagless interpreter

Intro toy interpreter and linter in haskell

* [Chapter 1: Simple abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [Chapter 2: Empowering the interpreter with mtl](https://github.com/soulomoon/arith/tree/master/arith2)
* [Chapter 3: Extension to the interpreter with adi](https://github.com/soulomoon/arith/tree/master/arith3)
* [Chapter 4: Tag the tagless interpreter](https://github.com/soulomoon/arith/tree/master/arith4)
* [Chapter 5: Regain the Linter with typeFamilies](https://github.com/soulomoon/arith/tree/master/arith4)
* ...

In the last chapter, we have expand the interpreter in a tagless style, but the price is losing 
the linter since we use concrete type notation for the interpreter type class.
In this chapter, we are going to regain the linter by using typeFamilies to abstract out
the interpreter type class.

## Polymorphic over the same type

Our target language now have two possible type, Bool and Int, and we want them to be type checked by the type system of haskell. 
But since typing for the function of linter(abstract interpreter) need different set of types compared to the concrete interpreter. 
Then we need our interpreter type class functions to have relative typing (relative to Bool and Int) differ by what type of interpreter is. 
This means we need to relate type to type. This is what type families does.

First we would want to define type for wether it is abstract and concrete. Using DataKinds we can do

```haskell
data InterpreterType = Abstract | Concrete
```

Then we build the type families for typing of our interpreter functions based on its type.
For a concrete interpreter, the representative type for type a is simply a (Int is simply Int).
For a abstract interpreter, everything is the symbolic type `Symbol`.

```haskell
type family Value (a :: *) (b :: InterpreterType) where
    Value a Concrete = a
    Value _ Abstract = Symbol
```

## Generify the Interpret

We need functions of the interpreter have a relative type depending on the output type and interpreter type.
`Value Bool t` or `Value Int t`

```haskell
class (Monad m) => InterpretB (t :: InterpreterType) m  where
    evalAnd :: (v ~ Value Bool t) => v -> v -> m v
    evalLitB :: (v ~ Value Bool t) => Bool -> m v

class (Monad m) => InterpretI (t :: InterpreterType) m where
    evalMul :: (v ~ Value Int t) => v -> v -> m v
    evalDiv :: (v ~ Value Int t) => v -> v -> m v
    evalLitI :: (v ~ Value Int t) => Int -> m v
```

## Generic eval

Thus we generify the Interpret. Now with a little caveat. The `evalMul` inside `eval` do not understand what `t :: InterpreterType` is has.
An explicit type application is needed. Eval is basically the same old one. Now it can be shared the linter too.

```haskell
eval :: forall t m v . (Interpret t m) => Combinator (Evaluator m v t)
eval ev (Mul x y  ) = hoistArgs (evalMul @t) (ev x) (ev y)
eval ev (Div x y  ) = hoistArgs (evalDiv @t) (ev x) (ev y)
eval ev (And x y  ) = hoistArgs (evalAnd @t) (ev x) (ev y)
eval ev (LitB a) = evalLitB @t a
eval ev (LitI  a) = evalLitI @t a
```

Now the same old Linter is back.

```haskell
instance (MonadLint m) => InterpretI Abstract m where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
        else return x
    evalLitI a = return $ if a == 0 then Zero else NotZero

instance (MonadLint m) => InterpretB Abstract m where
    evalAnd _ _ = return NotKnown
    evalLitB _ = return NotKnown
```
