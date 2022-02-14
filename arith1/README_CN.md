# 系列1: 简单的abstract and concrete Interpreter

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* 系列1: 简单的abstract and concrete Interpreter
* 系列2: 用mtl来增强interpreter
* 系列3: 用adi来写interpreter的extension

系列1的代码可以见[github源代码](https://github.com/soulomoon/arith/tree/master/arith1)

## 语言

前面的目标语言非常简单。仅支持乘法和除法，还有一个除以0的Exception以减少工作量。

## 简单的concrete interpreter

首先用algebraic data type和simple recursive decent写出最简单的arith interpreter。

```haskell
data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int

eval :: Expr -> Int
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Lit a) = a
```

## 简单的abstract interpreter

同样用最简单的方式写出一个linter. 能检查出最简单除以0的错误.

```haskell
data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
lint :: Expr -> Symbol
lint (Mul x y) =
    let xx = lint x
        yy = lint y
    in  if xx == NotKnown || yy == NotKnown
            then NotKnown
            else if xx == Zero || yy == Zero then Zero else NotZero
lint (Div x y) =
    let xx = lint x
        yy = lint y
    in  if yy == Zero || yy == NotKnown then NotKnown else xx
lint (Lit a) = if a == 0 then Zero else NotZero
```

## Class based polymorphic

可以看到`eval`和`lint`的结构都非常相似，稍微用type class用做一个ad-hoc polymorphism。

```haskell
class Interpret v where
    eval :: Expr -> v
    eval (Mul x y) = evalMul (eval x) (eval y)
    eval (Div x y) = evalDiv (eval x) (eval y)
    eval (Lit a) = evalLit a

    evalMul :: v -> v -> v
    evalDiv :: v -> v -> v
    evalLit :: Int -> v

instance Interpret Int where
    evalMul = (*)
    evalDiv = div
    evalLit = id

instance Interpret Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = NotKnown
                  | xx == Zero || yy == Zero         = Zero
                  | otherwise                        = NotZero
    evalDiv xx yy = if yy == Zero || yy == NotKnown then NotKnown else xx
    evalLit a = if a == 0 then Zero else NotZero
```

这里我们区分linter和interpreter的方式就是注明一下类型，我们会用`TypeApplications`来简化这个过程，避免写出完整的类型。

```haskell
execEval :: Expr -> IO ()
execEval = print . eval @Int
execLint :: Expr -> IO ()
execLint = print . eval @Symbol

main :: IO ()
main = do
    print "hello"
    execEval $ Lit 1
    execEval $ Mul (Lit 1) (Lit 2)
    execLint $ Mul (Lit 1) (Lit 2)

    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 1) (Lit 0))
    print "End"
```

然后我们可以来跑一下


```bash
❯ cabal run
Up to date
"hello"
NotZero
NotZero
NotZero
NotKnown
NotKnown
NotKnown
"End"
```
