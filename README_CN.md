# 好玩而且简单的haskell写的linter和interpreter系列

这个系列的文章会用haskell写点好玩而且简单的abstract interpreter(linter)和concrete interpreter(interpreter).

* [系列1: 简单的abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [系列2: 用mtl来增强interpreter](https://github.com/soulomoon/arith/tree/master/arith2)
* [系列3: 用adi来写interpreter的extension](https://github.com/soulomoon/arith/tree/master/arith3)

## 语言

前面的目标语言非常简单。仅支持乘法和除法，还有一个除以0的Exception。

```haskell
data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int
```

## 实现

实现尽量精简，尽量保持在100行以下。尽量开各种语言拓展来消除各种boilerplate code。


