# Chapter 4: Tag the tagless interpreter

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* 系列1: 简单的abstract and concrete Interpreter
* 系列2: 用mtl来增强interpreter
* 系列3: 用adi来写interpreter的extension
* 系列4: 用tagless的方式拓展interpreter.
* ...

[本篇的所有代码都在github](https://github.com/soulomoon/arith/tree/master/arith4).

在上一个篇中，我们写了an extendible effectful interpreter in mtl style而且用adi的办法来写拓展。
在这一篇中，我们会把我们的interpreter分割成tag的部分和不带tag的部分来提高拓展性.
大部分的技巧都来自著名的[tagless final](https://okmij.org/ftp/tagless-final/course/lecture.pdf),
我们会通过拓展我们的目标语言来展示这些技巧。

## 把tag的部分分离出来

在前面的`Interpret m v`, `eval` 负责把tagged的expression的tag处理掉, 然后扔到分立的evaluation function中`evalMul, evalDiv, evalLit`。所以它会带着tagged的类型，而同样在一个type class里面的其他function则不会。所以我们需要把`eval` 分离出来。
这样, 我们可以分离interpreter中`tagged`的部分和不带`tag`的部分。

* tagged 部分 `Expr` 和 `eval`.
* tagless 部分, `Interpret` type class和它的instance, 我们可以很方便地去拓展这部分。

## 拓展目标语言

我们会把我们的目标语言拓展到带boolean的。

```haskell
data Expr = ... | And Expr Expr | LitBool Bool
```

然后通过type class, 为新增加的部分建立新的interpreter `InterpretB`。然后我们会重命名旧的部分为`InterpretI`.
然后把他们用`ConstraintType`弄在一起。
这样还不够，因为目前`InterpretI m v`, `InterpretB m v` and `eval :: Combinator (Evaluator m v)`
都带有v, 除非我们用tag的方式，我们没有办法把这些`v`都给unify.
所以暂时干脆直接把`v`扔掉，然后用在里面用具体的类型。

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

## 把目标语言用GADTs来embed

`eval`需要返回一个通用的`m v` (根据不同的expression可能是`m Bool`或`m Int`).
目前的`Expr`没有提供到`v`的信息，是不够用的。
所以要给用`GADTs`来强化我们的`Expr`定义。
这样我们把目标语言embed到haskell里面去。还有好处就可以让`Expr`接受haskell的type check。

```haskell
data Expr v where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool
deriving instance (Show a) => Show (Expr a)
```

这样根据不同的constructor就可以phantom type `v`推导出来. 现在我们就准备好写`eval`了

```haskell
eval :: (Interpret m) => (Expr v -> m v) -> Expr v -> m v
eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
eval ev (LitI a) = evalLitI a
eval ev (LitB a) = evalLitB a
eval ev (And x y) = evalAnd (ev x) (ex y)
```

## 损失掉linter

我们把其他的function的类型修一修就可以拿到一个可以运行的concrete interpreter了
但这里因为我们的type class `Interpret*`的function用了具体类型来方便做tagless的interpreter, 我们损失掉linter。
下一个篇会重新把`Interpret*`类型抽象化来重新获得我们的linter。