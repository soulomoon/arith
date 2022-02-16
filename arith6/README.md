# Chapter 4: Tag the tagless interpreter

Intro toy interpreter and linter in haskell

* [Chapter 1: Simple abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [Chapter 2: Empowering the interpreter with mtl](https://github.com/soulomoon/arith/tree/master/arith2)
* [Chapter 3: Extension to the interpreter with adi](https://github.com/soulomoon/arith/tree/master/arith3)
* [Chapter 4: Tag the tagless interpreter](https://github.com/soulomoon/arith/tree/master/arith4)

In the last chapter, we implement an extendible effectful interpreter in mtl style along with methods in adi to write extensions.
In this chapter, we are providing more flexibility to the interpreter by splitting the interpreter into tag and tagless part.
The tagless part can be seen from the famous paper.
And we are going demonstrate this expanding the target language.

## Rip out the tag part

In the last `Interpret m v`, the `eval` function is responsible for extension and also ripping out the tagged expression
to untagged one and feed to the untagged evaluation function `evalMul, evalDiv, evalLit`.
It contains the tagged type, while the rest of the function in the same class does not. Sure we can drag out the `eval`
function.

```haskell
eval :: (Interpret m v) => Combinator (Evaluator m v)
eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
eval ev (Lit a) = evalLit a

class (Monad m) => Interpret m v where
    evalMul :: v -> v -> m v
    evalDiv :: v -> v -> m v
    evalLit :: Int -> m v
```

It split the whole interpreter into two parts: the tagged part and the tagless part.
In the tagged part `eval` and `Expr`, we still enjoys the tagged convenient such as deriving show class for the expression.
In the tagless part, `Interpret m v` and its instances, we can enjoy the tagless convenient too, such as expanding the interpreter without modifying the old class.

## Expanding the target language

Here we expand the language to contains more also some boolean expressions.

```haskell
data Expr a =
    Mul (Expr Int) (Expr Int)
    | Div (Expr Int) (Expr Int)
    | And (Expr Boolean) (Expr Int)
    | Or Expr Expr
    | Lit a deriving Show
```

then the extension to add source span to context.

```haskell
evAddSrc :: (MonadReader SrcSpan m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e
```

we could obtain the evaluator:

```haskell
extendedEval :: (Interpret m v, MonadReader SrcSpan m, MonadIO m) => Evaluator m v
extendedEval = fix $ evAddSrc eval
```

## Extension to trace the execution and stack up the extension

Same as adding source span
we can do a top down trace:

```haskell
evTrace :: (MonadIO m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = liftIO (print e) >> ev ev' e
```

also a bottom up trace:

```haskell
evTrace :: (MonadIO m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = ev ev' e <* liftIO (print e)
```

and of course we can stack the two extension together:

```haskell
extendedEval :: (Interpret m v, MonadReader SrcSpan m, MonadIO m) => Evaluator m v
extendedEval = fix $ evTrace $ evAddSrc eval
```
