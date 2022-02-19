# Catalog

Intro toy interpreter and linter in haskell

* [Chapter 1: Simple abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [Chapter 2: Empowering the interpreter with mtl](https://github.com/soulomoon/arith/tree/master/arith2)
* [Chapter 3: Extension to the interpreter with adi](https://github.com/soulomoon/arith/tree/master/arith3)
* [Chapter 4: Tag the tagless interpreter](https://github.com/soulomoon/arith/tree/master/arith4)
* [Chapter 5: Regain the Linter with typeFamilies](https://github.com/soulomoon/arith/tree/master/arith5)
* [Chapter 6: Expand the target language to have hof](https://github.com/soulomoon/arith/tree/master/arith6)
* [Chapter 7: recursion scheme over the target language](https://github.com/soulomoon/arith/tree/master/arith7)

## Language

The target language for the first 3 chapters is rather simple. It support multiplication and division in Int. And would have only exception that that is divide by Zero.
Since the focus is in the technique of writing interpreter in haskell in an extendible manner rather than language features.

```haskell
data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int
```

The target language for the chapters 4 5 with additional bool type.
To demonstrate expanding in tagless style

```haskell
data Expr a where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool
```

the target langue would be more useful with high order lambda function enabled in chapter 5.

```haskell
type VC v = (Show v, Typeable v)
data Expr a where
  Mul :: Expr Int -> Expr Int -> Expr Int
  Div :: Expr Int -> Expr Int -> Expr Int
  And :: Expr Bool -> Expr Bool -> Expr Bool
  LitI :: Int -> Expr Int
  LitB :: Bool -> Expr Bool
  Lam :: (VC c, VC b) => (b -> c) -> Expr (b -> c)
  App :: (VC a, VC b) => Expr (b -> a) -> Expr b -> Expr a
```




the taget language would be defined in a Gadt way as a higher order functor to fit recursion scheme.
and also with type families' polymorphic on its phantom type `v`.

```haskell
data ExprF (t :: InterpreterType) (f :: * -> *) :: * -> * where
  MulF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  AndF :: (v ~ Value Bool t) => f v -> f v -> ExprF t f v
  DivF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  LitIF :: Int -> ExprF t f (Value Int t)
  LitBF :: Bool -> ExprF t f (Value Bool t)
  LamF :: Value (b -> c) t -> ExprF t f (Value (b -> c) t)
  AppF :: (Show (Value b t)) => f (Value (b -> a) t) -> f (Value b t) -> ExprF t f (Value a t)
```

## Implementation

The implementation would be short and concise, now each of which is shorter than 100 lines of code.
And I would use language extension aggressively to enhance haskell, so the code would be minimal.

## Inspired by

* Typed taggles final interpreters
* hnix
* Abstracting Definitional Interpreters
