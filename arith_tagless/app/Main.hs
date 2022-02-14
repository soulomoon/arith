{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Identity

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)

class Arith m a where
    mul :: m a -> m a -> m a
    divv :: m a -> m a -> m a
    lit :: Int -> m a

newtype Exception = ExceptDivByZero String deriving (Eq, Show)
type MonadExec m =  (MonadReader Int m,  MonadError Exception m, Monad m)
type ValueExec = ReaderT Int (ExceptT Exception Identity)

instance (MonadExec m) => Arith m Int where
    mul = liftM2 (*)
    divv ma mb = do
        b <- mb
        if b == 0 then throwError (ExceptDivByZero "divider is zero") else liftM2 div ma mb
    lit = return


-- eval :: ValueExec String -> IO ()j
eval ::  ValueExec Int -> IO ()
eval a = print $ runReaderT a 1


p :: (Arith m a) => m a
p = lit 1

p2 :: (Arith m a) => m a
p2 = mul (lit 1) (lit 2)

p3 :: (Arith m a) => m a
p3 = mul (divv (lit 1) (lit 0)) (divv (lit 1) (lit 0))

main :: IO ()
main = do
    print "hello"
    eval p2
    eval p3
    print "End"

