# 系列5: 用typeFamilies重获Linter

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* 系列1: 简单的abstract and concrete Interpreter
* 系列2: 用mtl来增强interpreter
* 系列3: 用adi来写interpreter的extension
* 系列4: 用tagless的方式拓展interpreter.
* 系列5: 用typeFamilies重获Linter.
* 系列6: 拓展到高阶lambda function(working...).
* 系列7: 让目标语言用上free monad和recursion scheme(working...).

系列5的代码可以见[github源代码](https://github.com/soulomoon/arith/tree/master/arith5)

在上一个篇中，我们使得我们的interpreter可以跟随语言一起以tagless的方式拓展。
但是我们丧失了linter因为类型type class上的function的类型都是具体的。
在这一篇中，我们会用typeFamilies去要重新获得抽象化我们的type class的类型，来取回我们丧失的linter。

## Polymorphic over the same type

我们的目标语言现在有两种类型，Bool和Int，同时我们想要它可以被haskell的type system type check。
但是因为linter对类型的需求和concrete interpreter会不同。所以我们需要让我们的type class的function
拥有相对的类型（相对于Bool和Int）,同时可以被interpreter的种类区分开来。这意味着我们需要type到type的映射，而这就是
type families的能力。

首先我们用DataKinds定义interpreter的类型。

```haskell
data InterpreterType = Abstract | Concrete
```

然后我们定义type families去提供interpreter function的类型
对于concrete的interpreter，对于的类型分别是id Int和id Bool
对于abstract的interpreter(linter)，对于的类型分别是(Int -> SymbolI, Bool -> SymbolB)

```haskell
data SymbolI = Zero | NotZero | NotKnown deriving (Show, Eq)
data SymbolB = TrueOrFalse deriving (Show, Eq)
type family Value (a :: *) (b :: InterpreterType) where
  Value a Concrete = a
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
```

## Generify the Interpret

像下面这样Interpreter的function类型被重新抽象化了, 分别是`Value Bool t` or `Value Int t`

```haskell
class (Monad m) => InterpretB (t :: InterpreterType) m  where
    evalAnd :: (v ~ Value Bool t) => v -> v -> m v
    evalLitB :: (v ~ Value Bool t) => Bool -> m v

class (Monad m) => InterpretI (t :: InterpreterType) m where
    evalMul :: (v ~ Value Int t) => v -> v -> m v
    evalDiv :: (v ~ Value Int t) => v -> v -> m v
    evalLitI :: (v ~ Value Int t) => Int -> m v
```

## Generic eval

我们重新抽象化我们的interpreter之后，在`eval`中遇到了一点问题，就是`evalMul`不知道`t :: InterpreterType`,
需要显式的type application(还有别的地方也需要)。除此之外，基本上是原来的`Eval`, 只不过现在也可以用在Linter上了

```haskell
eval :: forall t m v . (Interpret t m) => Combinator (Evaluator m v t)
eval ev (Mul x y  ) = hoistArgs (evalMul @t) (ev x) (ev y)
eval ev (Div x y  ) = hoistArgs (evalDiv @t) (ev x) (ev y)
eval ev (And x y  ) = hoistArgs (evalAnd @t) (ev x) (ev y)
eval ev (LitB a) = evalLitB @t a
eval ev (LitI  a) = evalLitI @t a
```

原来的Linter可以被重新拿回来用了

```haskell
instance (MonadLint m) => InterpretI Abstract m where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
        else return x
    evalLitI a = return $ if a == 0 then Zero else NotZero

instance (MonadLint m) => InterpretB Abstract m where
    evalAnd _ _ = return TrueOrFalse
    evalLitB _ = return TrueOrFalse
```

我们可以来运行一下

```haskell
extendedEval :: forall t m v
     . (Interpret t m, MonadReader SrcSpan m, MonadIO m, Show v)
    => Evaluator m v t
extendedEval = fix $ (evTrace @t) $ (evAddSrc @t) (eval @t)
execLint :: (Show (Value v Abstract)) => Expr v -> IO ()
execLint expr = print =<< runWriterT
    ((`runReaderT` initSrc) $ extendedEval @Abstract @ValueLint expr)
expr :: Expr Int
expr = Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
main :: IO ()
main = execLint expr
```

结果还是原来的感觉

```
Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
Div (LitI 1) (LitI 0)
LitI 1
LitI 0
Div (LitI 2) (LitI 0)
LitI 2
LitI 0
(NotKnown,[ExceptDivByZero (SrcSpan (Div (LitI 1) (LitI 0))),ExceptDivByZero (SrcSpan (Div (LitI 2) (LitI 0)))])
```

## Forecasts

现在的目标语言还是很低级，所以下一篇，我们继续拓展我们的interpreter使得目标语言可以有higher order function。