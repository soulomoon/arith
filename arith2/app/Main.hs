{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main where
import           Control.Arrow                  ( (>>>) )
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Reader (ReaderT (runReaderT))

data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int deriving Show

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
newtype Exception a = ExceptDivByZero a deriving (Eq, Show)

hoistOut1 :: Monad m => (v -> v) -> (v -> m v)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (v -> v -> v) -> (v -> v -> m v)
hoistOut2 = (.) hoistOut1

hoistArgs :: forall m a b c. Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do aa <- a; bb <- b; f aa bb

class (Monad m) => Interpret m v where
    eval :: Expr -> m v
    eval (Mul x y) = hoistArgs evalMul (eval x) (eval y)
    eval (Div x y) = hoistArgs evalDiv (eval x) (eval y)
    eval (Lit a) = evalLit a

    evalMul :: v -> v -> m v
    evalDiv :: v -> v -> m v
    evalLit :: Int -> m v

data SrcSpan= SrcSpan Expr deriving (Show)
type MonadExec m =  (MonadReader SrcSpan m,  MonadError (Exception SrcSpan) m, Monad m)
type MonadLint m =  (MonadReader SrcSpan m, MonadWriter [Exception SrcSpan] m, Monad m)

instance (MonadExec m) => Interpret m Int where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = do
        src <- ask
        throwError (ExceptDivByZero src)
    evalDiv a b = hoistOut2 div a b
    evalLit = hoistOut1 id

instance (MonadLint m) => Interpret m Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = 
        if y == Zero
        then do 
            src <- ask
            tell [ExceptDivByZero src] >> return NotKnown
        else return x
    evalLit a = return $ if a == 0 then Zero else NotZero

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) Identity)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] Identity)

initSrc :: SrcSpan
initSrc = SrcSpan (Lit 0) 

execEval :: Expr -> IO ()
execEval = print . flip runReaderT initSrc . eval @ValueExec @Int
execLint :: Expr -> IO ()
execLint = print . flip runReaderT initSrc . eval @ValueLint @Symbol

main :: IO ()
main = do
    print "hello"
    execEval $ Lit 1
    execEval $ Mul (Lit 1) (Lit 2)
    execEval $ Mul (Lit 1) (Lit 2)
    execEval $ Div (Lit 1) (Lit 0)

    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 1) (Lit 0))
    print "End"

