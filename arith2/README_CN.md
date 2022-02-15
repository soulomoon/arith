# 系列2: 用mtl来增强interpreter

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* 系列1: 简单的abstract and concrete Interpreter
* 系列2: 用mtl来增强interpreter
* 系列3: 用adi来写interpreter的extension

系列2的代码可以见[github源代码](https://github.com/soulomoon/arith/tree/master/arith2)

在之前我们实现了一个非常简单无用的解释器然后我们来让它变得有用一些吧。让它拥有更多的功能和副作用。

## 把结果用monad包起来

在type class中把结果用monad包可以让我们运行各种副作用。

```haskell
class (Monad m) => Interpret m v where
    eval :: Expr -> m v
    eval (Mul x y) = hoistArgs evalMul (eval x) (eval y)
    eval (Div x y) = hoistArgs evalDiv (eval x) (eval y)
    eval (Lit a) = evalLit a

    evalMul :: v -> v -> m v
    evalDiv :: v -> v -> m v
    evalLit :: Int -> m v
```

## mtl style 依赖注入

前面的`Monad m`在instance中可以变得更多具体。
例如下面，解释器可以抛出异常，linter可以记录跑的过程中的各种错误.所以分别加上了`MonadError`和`MonadWriter`, 需要`ConstraintKinds`来完成。

```haskell
type MonadExec m =  (MonadError Exception m, Monad m)
type MonadLint m =  (MonadWriter [Exception] m, Monad m)

instance (MonadExec m) => Interpret m Int where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = throwError ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLit = hoistOut1 id

instance (MonadLint m) => Interpret m Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then tell [ExceptDivByZero] >> return NotKnown
        else return x
    evalLit a = return $ if a == 0 then Zero else NotZero
```

用mtl, 我们可以轻松把一堆monad堆起来形成一个具体的类型，来满足上面的`Constraint`需求。这个具体的类型能够注入到我们上面的interpreter和linter里面去, 像下面这样。

```haskell
type ValueExec = ExceptT Exception Identity
type ValueLint = WriterT [Exception] Identity

execEval :: Expr -> IO ()
execEval = print . eval @ValueExec @Int
execLint :: Expr -> IO ()
execLint = print . eval @ValueLint @Symbol
```

要添加更多的副作用操作，我们只需要给`MonadExec`和`MonadLint`加点`Constraint`，然后更新一下`ValueExec`和`ValueLint`来满足新的`Constraint`的需求。

## Add meta information for Exception

拿linter来举个例子给exception加点meta信息。

```haskell
data SrcSpan = SrcSpan Expr deriving (Show)
```

我们可以增加给我们抽象的monad `m`增加一个`MonadReader SrcSpan`constraint, 然后在过程中就可以用ask拿到这个信息去喂给`ExceptDivByZero`。

```haskell
type MonadExec m =  (MonadReader SrcSpan m,  MonadError (Exception SrcSpan) m, Monad m)

instance (MonadExec m) => Interpret m Int where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = do
        src <- ask
        throwError (ExceptDivByZero src)
    evalDiv a b = hoistOut2 div a b
    evalLit = hoistOut1 id
```

在具体的monad stack中，可以加一个`ReaderT`去满足这个constraint

```haskell
type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) Identity)
initSrc :: SrcSpan
initSrc = SrcSpan (Lit 0) 0 0
execEval :: Expr -> IO ()
execEval = print . flip runReaderT initSrc . eval @ValueExec @Int
```

同样的操作可以给到interpreter。目前我们`SrcSpan`永远没变化, 下一篇我们会利用一些技巧把这个信息注入到context中去
