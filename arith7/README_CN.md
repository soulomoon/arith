# Chapter 5: Regain the Linter with typeFamilies

Intro toy interpreter and linter in haskell

应该是最后一篇了，这一篇会安排上Gadts的recursion scheme。

recursion scheme over Gadts 可以从这个[blog](http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html)看到。
我们会使用类似的定义，
但我们的Gadts的值会用type families来做polymorphic, 使之可以fit linter的定义,
但不丢失原来的embedding的类型安全。
我们会redefine`chapter 3`的类似方式把额外的一层注入到recursion scheme中可以使得写
语言的extension简单。

这篇的recursion scheme的implementation的代码可以从这里看到(github repo)[]
因为结合前几篇一系列的功能会比较复杂，单纯的recursion scheme会更
加简单一点，可以看(simple version的recursion scheme)[]。

## Injective type families

因为类型推倒会变得非常复杂, 相对于对比前面的定义来说。所以我们需要把`Value` type family变成 injective type families来增强可推导性，主要是让其可以做两个类型domain的双射。

```haskell
type family Value (a :: *) (b :: InterpreterType) where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value (a -> b) Abstract = Value a Abstract -> Value b Abstract
  Value a Concrete = a 
```

## Functor version of target language

functor的目标定义使之符合recursion sheme的要求。

```haskell
data ExprF (t :: InterpreterType) (f :: * -> *) :: * -> * where
  MulF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  AndF :: (v ~ Value Bool t) => f v -> f v -> ExprF t f v
  LitIF :: Int -> ExprF t f (Value Int t)
  ...
```

`(ExprF t :: (* -> *) -> (* -> *))`就变成了饿一个rank2的functor了。

recursion scheme中的catamorphism在rank2 functor就会需要natural transformation(下面会提到)。

higher rank fixed type来帮助我们fix rank2的functor(`ExprF t`).

```haskell
newtype Fix f a = Fix { unFix :: f (Fix f) a }
type Expr t = Fix (ExprF t)
```

同时可以使用`pattern synonym`来让fixed的类型更加容易读和写。

```haskell
pattern And a  b = Fix (AndF a b)
{-# COMPLETE Mul, LitI, Div, Lam, App, LitB, And #-}
```

## Natural transformation

两个functor之间的morphism就是natural transformation。在haskell可以有如下的定义(hack增加了show可以更容易implement). 同时我们需要写一个functor2的fmap。

```haskell
type f ~> g = forall a .(Show a) => f a -> g a
class Functor2 (h :: (* -> *) -> * -> *) where
  map2:: Nat f g h
instance Functor2 (ExprF t) where
  map2 f (MulF a b) = MulF (f a) (f b)
  map2 f (LitIF i) = LitIF i
  ...
```

rank2的algebra，从`(ExprF t m) :: * -> *` to `m :: * -> *`。

```haskell
evalAlg :: forall t m. (Interpret t m) => ExprF t m ~> m
```

## catamorphism and its transformation

我们会稍微修改一个catamorphism方便做额外的层的injection, 基本上就是第三篇中提到的adi的recursion theorem的版本。

```haskell
cata :: forall h f. (Functor2 h) => Transform f h -> (h f ~> f) -> (Fix h ~> f)
cata f alg = f (alg . map2 (cata f alg) . unFix)
```

`eval` 的定义如下， 把`Expr t` 到 `m`的natural transformation。
.

```haskell
eval :: forall t m a. (Interpret t m, MonadIO m, MonadReader SrcSpan m) => Expr t ~> m
eval = cata (evAddSrc . evTrace) (evalAlg @t @m)
```

我们可以稍微修改一下之前的extension来做我们的transformation给每一层增加一些东西。

```haskell
type Transform f h = forall a.(Show a) => (Fix h a -> f a) -> (Fix h a -> f a)
evAddSrc :: (MonadIO m, Interpret t m, MonadReader SrcSpan m) => (Transform m (ExprF t))
evAddSrc f e = local (const $ SrcSpan (show e)) $ f e
```

## 跑一跑

然后我们可以用上之前的定义，改一改类型, 就可以继续跑了。

```haskell
execEval :: Show a => Expr 'Concrete a -> IO ()
execEval expr = runExceptT ((`runReaderT` SrcSpan "") $ eval @Concrete @ValueExec expr) >>= print

execLint :: Show a => Expr 'Abstract a -> IO ()
execLint expr = runWriterT ((`runReaderT` SrcSpan "") $ eval @Abstract @ValueLint expr) >>= print


main :: IO ()
main = do
  execEval (Mul (Div (LitI 2) (LitI 0)) (Div (LitI 4) (LitI 0))) 
  execLint (Mul (Div (LitI 2) (LitI 0)) (Div (LitI 4) (LitI 0))) 
```
