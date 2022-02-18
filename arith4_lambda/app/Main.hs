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
{-# LANGUAGE RankNTypes #-}

module Main where
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Control.Monad.Writer
import Data.Data (typeOf)
import Data.Typeable (Typeable)

type VC v = (Show v, Typeable v)

data Expr a where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool
    Inj :: (VC a) => a -> Expr a
    Lam :: (VC c, VC b) => (b -> c) -> Expr (b -> c)
    App :: (VC a, VC b) => Expr (b -> a) -> Expr b -> Expr a


instance (Typeable a, Typeable b) => Show (a -> b) where
    show x = "(" ++ show (typeOf x) ++ ")"

deriving instance (Show a, Typeable a) => Show (Expr a)

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
newtype Exception a = ExceptDivByZero a deriving (Eq, Show)

hoistOut1 :: Monad m => (a -> b) -> (a -> m b)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
hoistOut2 = (.) hoistOut1

hoistArgs2 :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs2 f a b = do aa <- a; b >>= f aa

type (Interpret m ) = (InterpretI m, InterpretB m)

type Eval m = (forall u. (VC u) => (Expr u -> m u))
type EvalCombinator m = Combinator (Eval m)

eval :: forall m. (Interpret m) => Eval m -> Eval m
eval ev (Mul x y) = hoistArgs2 evalMul (ev x) (ev y)
eval ev (Div x y) = hoistArgs2 evalDiv (ev x) (ev y)
eval ev (LitI a) = evalLitI a
eval ev (LitB a) = evalLitB a
eval ev (And x y) = hoistArgs2 evalAnd (ev x) (ev y)
eval ev (Lam f) = evalLam f
eval ev (App f x) = hoistArgs2 evalApp (ev f) (ev x)
eval ev (Inj x) = return x

class (Monad m) => InterpretB m where
    evalAnd :: Bool -> Bool -> m Bool
    evalLitB :: Bool -> m Bool

class (Monad m) => InterpretI m where
    evalMul :: Int -> Int -> m Int
    evalDiv :: Int -> Int -> m Int
    evalLitI :: Int -> m Int
    evalLam :: (a -> b) -> m (a -> b)
    evalApp :: (a -> b) -> a -> m b

data SrcSpan = forall v . (VC v) => SrcSpan v
deriving instance Show SrcSpan
type MonadExec m = ( MonadIO m , MonadReader SrcSpan m , MonadError (Exception SrcSpan) m , Monad m)
type MonadLint m = ( MonadIO m , MonadReader SrcSpan m , MonadWriter [Exception SrcSpan] m , Monad m)

instance (MonadExec m) => InterpretI m where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = ask >>= throwError . ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLitI = hoistOut1 id
    evalLam = hoistOut1 id
    evalApp = hoistOut2 ($)

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


evAddSrc :: (MonadIO m, MonadReader SrcSpan m) => EvalCombinator m -> EvalCombinator m
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace :: (MonadIO m, Interpret m) => EvalCombinator m -> EvalCombinator m
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval :: (MonadIO m, MonadReader SrcSpan m, MonadError (Exception SrcSpan) m) => Eval m
extendedEval = fixEval $ evTrace $ evAddSrc eval

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (LitI 0)


-- linear space fix function since fold along evaluation would return different type of
-- values at each layer. Thus the cache one would fail to type check.
fixEval :: (Eval m -> Eval m) -> Eval m
fixEval ev = ev (fixEval ev)



execEval :: forall v. (VC v) => Expr v -> IO ()
execEval expr = print =<< runExceptT
    ((`runReaderT` initSrc) $ extendedEval expr)

-- execLint :: (Show v) => Expr v -> IO ()
-- execLint expr = print =<< runWriterT
--     ((`runReaderT` initSrc) $ extendedEval @ValueLint @Symbol expr)
-- a :: ValueExec Int
-- a = execEval $ Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
toBool :: Int -> Bool
toBool 0 = False
toBool _ = True



expr1 :: Expr Bool
expr1 = App (Lam toBool) (LitI 1)

main :: IO ()
main = do
    execEval $ Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))
    execEval expr1




