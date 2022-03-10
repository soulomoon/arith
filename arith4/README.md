# Chapter 4: Tag the tagless interpreter

All the source code showing in the chapter would be in [Chapter 4: Tag the tagless interpreter](https://github.com/soulomoon/arith/tree/master/arith4).

In the last chapter, we implement an extendible effectful interpreter in mtl style along with methods in adi to write extensions.
In this chapter, we are providing more flexibility to the interpreter by splitting the interpreter into tag and tagless part.
Most of the technique is coming from the famous [tagless final](https://okmij.org/ftp/tagless-final/course/lecture.pdf),
And we are going demonstrate the approach in expanding the target language.

## Rip out the tag part

In the last `Interpret m v`, the `eval` function is responsible for extension and also ripping out the tagged expression
to untagged one and feed to the untagged evaluation function `evalMul, evalDiv, evalLit`.
It contains the tagged type, while the rest of the function in the same class does not. Sure we can drag out the `eval`
function.

It splits the whole interpreter into two parts: the tagged part and the tagless part.

* In the tagged part `Expr`, we still enjoys the tagged convenient such as deriving show class for the expression.
* In the tagless part, `Interpret m v` and its instances, we can enjoy the tagless convenient too, such as expanding the interpreter without modifying the old type class.
`Eval` would act as the the bridge to connect them.

## Expanding the target language

Here we expand the language to contains also some boolean expressions.

```haskell
data Expr = ... | And Expr Expr | LitBool Bool
```

Then we can defined new part of the interpreter with new type class, `InterpretB`,
Here we rename the old `Interpret` type class to `InterpretI`. Then use `ConstraintType`
to Combine the two type back to `Interpret`.

But it is still not enough, With the type `InterpretI m v`, `InterpretB m v` and `eval :: Combinator (Evaluator m v)` all the relevant functions output `m v`.
But unification of these `v` would not be possible without `v` being a type with with explicit tagging.
At the moment, we remove `v` and fall back to use a concrete type as in the paper.

```haskell
type (Interpret m ) = (InterpretI m, InterpretB m)
class (Monad m) => InterpretB m where
    evalAnd :: Bool -> Bool -> m Bool
    evalLitB :: Bool -> m Bool
class (Monad m) => InterpretI m where
    evalMul :: Int -> Int -> m Int
    evalDiv :: Int -> Int -> m Int
    evalLitI :: Int -> m Int
```

## Embedded the language with GADTs

Now the `eval` would need to be generic enough to return `m v` (both `m Bool` and `m Int` based on different expression).
But the current `Expr` definition does not provide enough typing information differ what the `v` is.
In order for the polymorphic `v` in `eval` could be unified, we need additional typing information
to be carried by `Expr`. We need to use `GADTs` extension to boost the haskell type system.
What we want here is to embedded the target language into haskell.

```haskell
data Expr v where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool
deriving instance (Show a) => Show (Expr a)
```

The phantom type `v` in `Expr` could be inferred by different type constructor. It would be now sufficient to write `eval`.

```haskell
eval :: (Interpret m) => (Expr v -> m v) -> Expr v -> m v
eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
eval ev (LitI a) = evalLitI a
eval ev (LitB a) = evalLitB a
eval ev (And x y) = evalAnd (ev x) (ex y)
```

## Losing the linter

After fixing up other places a bit, we get a working concrete interpreter.
But We lose the linter, because in the type class `Interpret*`,
we use concrete type for function's output and input to make the transformation to a tagless style easier.
The type class now forcing concrete interpretation. Symbolic interpretation support is lost.
The way to re-generify the `interpreter*` and gaining back the symbolic interpreter would be in the next chapter.
