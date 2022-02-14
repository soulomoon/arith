# arith

Intro toy interpreter and linter in haskell

* [Chapter 1: Simple abstract and concrete Interpreter](https://github.com/soulomoon/arith/tree/master/arith1)
* [Chapter 2: Empowering the interpreter with mtl](https://github.com/soulomoon/arith/tree/master/arith2)
* [Chapter 3: Extension to the interpreter with adi](https://github.com/soulomoon/arith/tree/master/arith3)

## Language

The target language is rather simple. It support multiplication and division in Int. And would have only exception that that is divide by Zero.
Since the focus is in the technique of writing interpreter in haskell in an extendible manner rather than language features.

```haskell
data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int
```
