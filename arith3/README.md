# Chapter 3: Extension to the interpreter

In the last chapter, we implement an extendible effectful interpreter in mtl style.
In this chapter we are enhancing the interpreter using technique in paper [Abstracting Definitional Interpreters
](https://arxiv.org/abs/1707.04755) to write extensions. The basic idea is to insert an additional layer in the evaluation in every recursion.

## Explicit recursion

The evaluation should be revised explicit recursion style.
It open a window for injecting additional layer into the evaluation process.

```haskell
eval :: (Expr -> m v) -> Expr -> m v
eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
eval ev (Lit a) = evalLit a
```

To obtain the old plain evaluator, we can just `fix eval` where fix the y combinator in lambda calculus.

## Extension to insert the source span into context

Inserting the source span into context (more specifically the monad reader) could be written as an extension in a combinator style.
First some handy type synonym

```haskell
type Evaluator m v = Expr -> m v
type Combinator a = a -> a
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
