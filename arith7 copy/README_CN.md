# Chapter 6: 语言HOF

在这一篇中，我们会拓展我们的语言到有Higher order function的程度.
大部分新增代码的是要让linter保持工作。因为语言是embed到haskell的所以concrete interpreting 
的部分可以复用haskell的。

## lambada function and application

给原来的Expr的基础上给Lam和App做一个简单的embedding。

```haskell
data Expr a where
  ...
  Lam :: (VC c, VC b) => (b -> c) -> Expr (b -> c)
  App :: (VC a, VC b) => Expr (b -> a) -> Expr b -> Expr a
```

首先我们需要把类型映射拓展到function。

```haskell
type family Value (a :: *) (b :: InterpreterType) where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value (a -> b) Abstract = Value a Abstract -> Value b Abstract
  Value a Concrete = a 
```

然后ad-hoc polymorphic的办法去跟据类型获得Symbol

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

为了可以让function 有一个简单的show class，我们会需要type是typeable把function type的string format拿到。

```haskell
instance (Typeable a, Typeable b) => Show (a -> b) where
  show x = "(" ++ show (typeOf x) ++ ")"
deriving instance (Show a, Typeable a) => Show (Expr a)
```

## Tagless expand

就像之前的那样，可以tagless地拓展interpreter， 再加上一个`InterpretL`来interpret lambda和application。

```haskell
type Interpret t m = (InterpretI t m, InterpretB t m, InterpretL t m)

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

修正下`Eval`的类型，在recursion的每一层可以有不同的`v`。
haskell类型推导系统的限制，`app`的eval需要一些额外的类型传递。

```haskell
type Eval t m = forall v. (VC v, Symbolic v, Interpret t m) => Expr v -> m (Value v t)
type Combinator a = a -> a
type EvalCombinator t m = Combinator (Eval t m)
eval :: forall t m. EvalCombinator t m
eval ev expr = case expr of
  ...
  (App (f :: Expr (b -> v)) (x :: Expr b)) ->
    hoistArgs (evalApp @t @m @v @b) (ev f) (ev x)
```

原来的cache fix function就用不了，因为每次的`v`都可能不一样。我们自己写一个简单的不cache的fix function。

```haskell
fixEval :: forall m t. (Eval m t -> Eval m t) -> Eval m t
fixEval ev = ev (fixEval @m @t ev)
```

## HOF

现在我们的interpterer就可以来interpter带高阶function的expression了。

```haskell
add1 :: Int -> Int
add1 = (+ 1)

double :: (Int -> Int) -> (Int -> Int)
double f = f . f

expr0 :: Expr Int
expr0 = App (App (Lam double) (Lam add1)) (LitI 1)
```
