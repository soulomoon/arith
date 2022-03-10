# Chapter 6: Expand the target language to have hof

In this chapter, we are going to expand the language to have hof.

## Expand the Value

We need to fix up the Value to have function type

```haskell
type family Value (a :: *) (b :: InterpreterType) where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value (a -> b) Abstract = Value a Abstract -> Value b Abstract
  Value a Concrete = a 
```

## Default Symbol

and a ad-hoc polymorphic way to return symbol based on type.

```haskell
class Symbolic a where
  defaultSym ::  Value a Abstract 
instance Symbolic Int where
  defaultSym = NotKnown
instance Symbolic Bool where
  defaultSym = TrueOrFalse
instance (Symbolic a , Symbolic b) => Symbolic (a -> b) where
  defaultSym _ = defaultSym @b
```

## Expand Expr

Show class for function for ease of debugging. The typeable constraint is needed because we want to
convert the function type to string.

```haskell
instance (Typeable a, Typeable b) => Show (a -> b) where
  show x = "(" ++ show (typeOf x) ++ ")"
deriving instance (Show a, Typeable a) => Show (Expr a)
```

Then we need to add symbolic and, Typeable constraint for Expr and add function type

```haskell
type VC v = (Show v, Typeable v, Symbolic v)
data Expr a where
  ...
  Lam :: (VC c, VC b) => (b -> c) -> Expr (b -> c)
  App :: (VC a, VC b) => Expr (b -> a) -> Expr b -> Expr a
```

## Tagless expand

As demonstrated, we can expand the Interpreter in tagless style,
type Interpret t m = (InterpretI t m, InterpretB t m, InterpretL t m)

```haskell
class (Monad m) => InterpretL (t :: InterpreterType) m where
  evalLam :: (v ~ Value (b -> a) t, Symbolic a) => (b -> a) -> m v
  evalApp :: forall a b. Value (b -> a) t -> Value b t -> m (Value a t)

instance (MonadExec m) => InterpretL Concrete m where
  evalLam = hoistOut1 id
  evalApp = hoistOut2 ($)

instance (MonadLint m) => InterpretL Abstract m where
  evalLam (_ :: b -> a) = return (const $ defaultSym @a)
  evalApp ap b = return $ ap b
```

## Fix the eval

we need to alter the signature of `Eval` a bit, to enable it to have different `v` at each layer of the recursion.

```haskell
type Eval t m = forall v. (VC v, Symbolic v, Interpret t m) => Expr v -> m (Value v t)
type Combinator a = a -> a
type EvalCombinator t m = Combinator (Eval t m)
```

And some more explicit type passing because the inference of haskell is not powerful enough to do
in `eval`.

```haskell
eval :: forall t m. EvalCombinator t m
eval ev expr = case expr of
  ...
  (App (f :: Expr (b -> v)) (x :: Expr b)) ->
    hoistArgs (evalApp @t @m @v @b) (ev f) (ev x)
```

linear space fix function since fold along evaluation would return different type of
values at each layer. Thus the cache one would fail to type check.

```haskell
fixEval :: forall m t. (Eval m t -> Eval m t) -> Eval m t
fixEval ev = ev (fixEval @m @t ev)
```

## HOF

Now our interpreter is capable of interpreting some higher order function.

```haskell
add1 :: Int -> Int
add1 = (+ 1)

double :: (Int -> Int) -> (Int -> Int)
double f = f . f

expr0 :: Expr Int
expr0 = App (App (Lam double) (Lam add1)) (LitI 1)
```
