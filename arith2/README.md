# Chapter 2: Empowering the interpreter with mtl

In chapter1, we implement a very naive(useless) abstract and concrete interpreter.
We are empowering it with functionalities to perform various effectful operations in mtl-style.

## Wrap output under monad

Wrap return type with an `monad m` by adding one more argument to Interpreter class for all possible effect that can be carried out by monad.

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

## mtl style dependency injection

Monad m would be bounded more concretely in the instances.
Concrete interpreter can throw exception, and abstract interpreter could record all the errors it found.

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

With mtl, we can easily build monad stack that fits the constraints and inject into the abstract interpreter in the following way.

```haskell
type ValueExec = ExceptT Exception Identity
type ValueLint = WriterT [Exception] Identity

execEval :: Expr -> IO ()
execEval = print . eval @ValueExec @Int
execLint :: Expr -> IO ()
execLint = print . eval @ValueLint @Symbol
```

To add more effectful operations, we just need to add more monad constraints to `MonadExec` and build a concrete monad stack using mtl that fit the constraints.

## Add meta information for Exception

Take the concrete interpreter for example
We could add some meta information for exception.

```haskell
data SrcSpan = SrcSpan Expr deriving (Show)
```

Then we add a `MonadReader SrcSpan` to constraint the abstract monad `m`. And retrieve the
`srcSpan` information when needed.

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

In the concrete monad stack, we stack a `ReaderT` on the top to full fill the constraint.

```haskell
type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) Identity)
initSrc :: SrcSpan
initSrc = SrcSpan (Lit 0) 0 0
execEval :: Expr -> IO ()
execEval = print . flip runReaderT initSrc . eval @ValueExec @Int
```

The same would goes for abstract interpreter.
The current source span in the context never changed, we shall insert the source span to the context in the next chapter.
