{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

module Main where
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Control.Monad.Writer

data Expr a where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool


deriving instance (Show a) => Show (Expr a)

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
newtype Exception a = ExceptDivByZero a deriving (Eq, Show)

hoistOut1 :: Monad m => (v -> v) -> (v -> m v)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (v -> v -> v) -> (v -> v -> m v)
hoistOut2 = (.) hoistOut1

hoistArgs2 :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs2 f a b = do aa <- a; b >>= f aa

type (Interpret m ) = (InterpretI m, InterpretB m)

eval :: (Interpret m) => (Expr v -> m v) -> Expr v -> m v
eval ev (Mul x y) = hoistArgs2 evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs2 evalDiv (ev x) (ev y)
eval ev (LitI a) = evalLitI a
eval ev (LitB a) = evalLitB a
eval ev (And x y) = hoistArgs2 evalAnd (ev x) (ev y)

class (Monad m) => InterpretB m where
    evalAnd :: Bool -> Bool -> m Bool
    evalLitB :: Bool -> m Bool

class (Monad m) => InterpretI m where
    evalMul :: Int -> Int -> m Int
    evalDiv :: Int -> Int -> m Int
    evalLitI :: Int -> m Int

data SrcSpan = forall v . (Show v) => SrcSpan v
deriving instance Show SrcSpan
type MonadExec m = ( MonadIO m , MonadReader SrcSpan m , MonadError (Exception SrcSpan) m , Monad m)
type MonadLint m = ( MonadIO m , MonadReader SrcSpan m , MonadWriter [Exception SrcSpan] m , Monad m)

instance (MonadExec m) => InterpretI m where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = ask >>= throwError . ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLitI = hoistOut1 id

instance (MonadExec m) => InterpretB m where
  evalAnd = hoistOut2 (&&)
  evalLitB = hoistOut1 id

-- instance (MonadLint m) => InterpretI m where
--     evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
--                   | xx == Zero || yy == Zero         = return Zero
--                   | otherwise                        = return NotZero
--     evalDiv x y = if y == Zero
--         then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
--         else return x
--     evalLit a = return $ if a == 0 then Zero else NotZero

type Evaluator m v = Expr v -> m v
type Combinator a = a -> a

evAddSrc :: (MonadReader SrcSpan m, Interpret m, Show v) => Combinator (Combinator (Evaluator m v))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace :: (MonadIO m, Interpret m, Show v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval :: (Interpret m, MonadReader SrcSpan m, MonadIO m, Show v) => Evaluator m v
extendedEval = fix $ evTrace $ evAddSrc eval

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (LitI 0)

execEval :: (Show v) => Expr v -> IO ()
execEval expr = print =<< runExceptT
    ((`runReaderT` initSrc) $ extendedEval @ValueExec expr)

-- execLint :: Expr -> IO ()
-- execLint expr = print =<< runWriterT
--     ((`runReaderT` initSrc) $ extendedEval @ValueLint @Symbol expr)

main :: IO ()
main = do
    execEval $ Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
    execEval $ And (LitB True) (LitB False)
    

