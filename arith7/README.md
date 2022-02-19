# Chapter 5: Regain the Linter with typeFamilies

Intro toy interpreter and linter in haskell

* [Chapter 1: Simple abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [Chapter 2: Empowering the interpreter with mtl](https://github.com/soulomoon/arith/tree/master/arith2)
* [Chapter 3: Extension to the interpreter with adi](https://github.com/soulomoon/arith/tree/master/arith3)
* [Chapter 4: Tag the tagless interpreter](https://github.com/soulomoon/arith/tree/master/arith4)
* [Chapter 5: Regain the Linter with typeFamilies](https://github.com/soulomoon/arith/tree/master/arith5)
* [Chapter 6: Expand the target language to have hof](https://github.com/soulomoon/arith/tree/master/arith6)
* [Chapter 7: a polymorphic typed recursion scheme over the Gadts](https://github.com/soulomoon/arith/tree/master/arith7)
  
In this chapter, we are going to take use of recursion scheme over the Gadts,
definition for the target language.
The Gadts constructors definition would be a polymorphic typed using the type families
to represent the underline type.
The recursion scheme over Gadts is described from this [blog](http://www.timphilipwilliams.com/posts/2013-01-16-fixing-gadts.html)
We use the same implementation
but attaching with `Value` type families to make the language more polymorphic so our linter fit.
and also attaching the injective layer for as in `chapter 3` but in a recursion scheme manner,
it can also be found in the hnix code base.

## Injective type families

Since the type inference would be relatively more complicated than the previous implementation.
We need to refine the the `Value` type families into injective type families to enable bidirectional
inference on left and right hand side.

```haskell
type family Value (a :: *) (b :: InterpreterType) where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value (a -> b) Abstract = Value a Abstract -> Value b Abstract
  Value a Concrete = a 
```

## Functor version of target language

We expand the expression into functor version to use with recursion scheme. It is also polymorphic
over interpreter type to enable different sets of parameters to be used in linter.

```haskell
data ExprF (t :: InterpreterType) (f :: * -> *) :: * -> * where
  MulF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  AndF :: (v ~ Value Bool t) => f v -> f v -> ExprF t f v
  LitIF :: Int -> ExprF t f (Value Int t)
  ...
```

It makes `(ExprF t :: (* -> *) -> (* -> *))` a functor of the functor category(rank2 functor).
In recursion scheme, the catamorphism over the rank2 functor would need morphism between functor to fold.
which is natural transformation.

The under higher rank fixed type would enable us to fix the rank2 functor (ExprF t).

```haskell
newtype Fix f a = Fix { unFix :: f (Fix f) a }
type Expr t = Fix (ExprF t)
```

we also use some pattern synonym to make the fixed version of AST more readable and could be written more friendly.

```haskell
pattern And a  b = Fix (AndF a b)
{-# COMPLETE Mul, LitI, Div, Lam, App, LitB, And #-}
```

## Natural transformation

a morphism between functor is called natural transformation which in haskell would normally be write as(little hack to implement the show class).
also we need to implement the rank2 functor.

```haskell
type f ~> g = forall a .(Show a) => f a -> g a
class Functor2 (h :: (* -> *) -> * -> *) where
  map2:: Nat f g h
instance Functor2 (ExprF t) where
  map2 f (MulF a b) = MulF (f a) (f b)
  map2 f (LitIF i) = LitIF i
  ...
```

a rank2 algebra for from `ExprF t m` to `m`.

```haskell
evalAlg :: forall t m. (Interpret t m) => ExprF t m ~> m
```

## catamorphism and its transformation

The slightly alter catamorphism for the rank2 algebra.
We alter it so the injection of additional layer to transform the folding could be perform,
an altered adi way from the previous chapter adjust for the recursion theorem.

```haskell
cata :: forall h f. (Functor2 h) => Transform f h -> (h f ~> f) -> (Fix h ~> f)
cata f alg = f (alg . map2 (cata f alg) . unFix)
```

the evaluation function is the following, fold the `Expr t` -> `m`, which is also natural transformation,
but we need the constraint the `a`
.

```haskell
eval :: forall t m a. (Interpret t m, MonadIO m, MonadReader SrcSpan m) => Expr t ~> m
eval = cata (evAddSrc . evTrace) (evalAlg @t @m)
```

We can have previous transformation of our interpreter but with a slightly altered typing.

```haskell
type Transform f h = forall a.(Show a) => (Fix h a -> f a) -> (Fix h a -> f a)
evAddSrc :: (MonadIO m, Interpret t m, MonadReader SrcSpan m) => (Transform m (ExprF t))
evAddSrc f e = local (const $ SrcSpan (show e)) $ f e
```

## Conclusion

The implementation is rather difficult and complex in the current implementation with heavily typing annotation.
But I did have a joy implementation it.
