# 系列3: 用adi来写interpreter的extension

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* 系列1: 简单的abstract and concrete Interpreter
* 系列2: 用mtl来增强interpreter
* 系列3: 用adi来写interpreter的extension

系列3的代码可以见[github源代码](https://github.com/soulomoon/arith/tree/master/arith3)

前面我们用mtl的风格实现了一个可以拓展出各种副作用的interpreter。下面我们继续增强我们的interpreter, 运用到的技巧在这里[Abstracting Definitional Interpreters
](https://arxiv.org/abs/1707.04755)。 adi可以让我们写拓展的方式来写解释器，办法就是在evaluation的过程中注入额外的一层。
这个过程有点像python的decorator还有后端的middleware的做法。

## 显式递归

首先我们得把原来的`eval`改成现式的，这可以使得在evaluation的过程中注入额外的一层成为可能。

```haskell
eval :: (Expr -> m v) -> Expr -> m v
eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
eval ev (Lit a) = evalLit a
```

去获得原来的interpreter,我们只需要`fix eval`(这里的fix是lambda calculus里面的y combinator)

## 把source span信息注入到context的Extension

上一篇中提到把source span的信息注入context(MonadReader)中，我们可以用写combinator形式来写extension。
首先我们写点type synonym来避免过长的类型。

```haskell
type Evaluator m v = Expr -> m v
type Combinator a = a -> a
```

然后这样我们就可以写出extension把source span的信息加进去。

```haskell
evAddSrc :: (MonadReader SrcSpan m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e
```

让`extendedEval`把`eval`吃掉, 然后再fix一下就好了。

```haskell
extendedEval :: (Interpret m v, MonadReader SrcSpan m, MonadIO m) => Evaluator m v
extendedEval = fix $ evAddSrc eval
```

## trace的Extension和把extension堆起来

就像前面那样，我们可以写extension来top down trace 。

```haskell
evTrace :: (MonadIO m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = liftIO (print e) >> ev ev' e
```

同样bottom up trace:

```haskell
evTrace :: (MonadIO m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = ev ev' e <* liftIO (print e)
```

当然我们可以把两个extensions组合起来一起用，又注入source span, 又trace。

```haskell
extendedEval :: (Interpret m v, MonadReader SrcSpan m, MonadIO m) => Evaluator m v
extendedEval = fix $ evTrace $ evAddSrc eval
```

然后我们来跑一下可以做trace的linter。

```haskell
execLint :: Expr -> IO ()
execLint expr = print =<< runWriterT
    ((`runReaderT` initSrc) $ extendedEval @ValueLint @Symbol expr)
main :: IO ()
main = execEval $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 2) (Lit 0))
```

得到top down的trace结果还有linter得到的一些信息：

```haskell
❯ cabal run
Up to date
Mul (Div (Lit 1) (Lit 0)) (Div (Lit 2) (Lit 0))
Div (Lit 1) (Lit 0)
Lit 1
Lit 0
Div (Lit 2) (Lit 0)
Lit 2
Lit 0
(NotKnown,[ExceptDivByZero (SrcSpan (Div (Lit 1) (Lit 0))),ExceptDivByZero (SrcSpan (Div (Lit 2) (Lit 0)))])
```
