# Chapter 5: Regain the Linter with typeFamilies

In the last chapter, we make it possible to expand the interpreter along with the language in a tagless style, but the price is losing the linter since we use concrete type notation for the interpreter type class.
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
For a abstract interpreter, (Int -> SymbolI, Bool -> SymbolB).

```haskell
type family Value (a :: *) (b :: InterpreterType) where
  Value a Concrete = a
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  data SymbolI = Zero | NotZero | NotKnown deriving (Show, Eq)
data SymbolB = TrueOrFalse deriving (Show, Eq)
```

## Generify the Interpret

Functions of the interpreter could now have a relative type depending on the output type and interpreter type, it is being re-generify as in the previous chapters.
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
An explicit type application is needed. Eval is basically the same old one. It can be used by linter too.

```haskell
eval :: forall t m v . (Interpret t m) => Combinator (Evaluator m v t)
eval ev (Mul x y  ) = hoistArgs (evalMul @t) (ev x) (ev y)
eval ev (Div x y  ) = hoistArgs (evalDiv @t) (ev x) (ev y)
eval ev (And x y  ) = hoistArgs (evalAnd @t) (ev x) (ev y)
eval ev (LitB a) = evalLitB @t a
eval ev (LitI  a) = evalLitI @t a
```

Now the same old Linter is back with additional InterpretB instance.
Nothing much to do here, because basically all the type check is embedded into haskell type system.

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
    evalAnd _ _ = return TrueOrFalse
    evalLitB _ = return TrueOrFalse
```

with a little type fixed up, now we could run the linter again.

```haskell
extendedEval :: forall t m v
     . (Interpret t m, MonadReader SrcSpan m, MonadIO m, Show v)
    => Evaluator m v t
extendedEval = fix $ (evTrace @t) $ (evAddSrc @t) (eval @t)
execLint :: (Show (Value v Abstract)) => Expr v -> IO ()
execLint expr = print =<< runWriterT
    ((`runReaderT` initSrc) $ extendedEval @Abstract @ValueLint expr)
expr :: Expr Int
expr = Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
main :: IO ()
main = execLint expr
```

the same old result.

```
Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
Div (LitI 1) (LitI 0)
LitI 1
LitI 0
Div (LitI 2) (LitI 0)
LitI 2
LitI 0
(NotKnown,[ExceptDivByZero (SrcSpan (Div (LitI 1) (LitI 0))),ExceptDivByZero (SrcSpan (Div (LitI 2) (LitI 0)))])
```


## Forecasts

The target language now is till very native.
We would expand the language to have an higher order function and expand along with the the interpreter the next chapter.
