{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.RWS
  ( MonadReader (ask, local),
    MonadWriter (writer),
  )
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.Writer
import Data.Data (Typeable)
import Data.Typeable (typeOf)

type VC v = (Show v, Typeable v)

data Expr a where
  Mul :: Expr Int -> Expr Int -> Expr Int
  Div :: Expr Int -> Expr Int -> Expr Int
  And :: Expr Bool -> Expr Bool -> Expr Bool
  LitI :: Int -> Expr Int
  LitB :: Bool -> Expr Bool
  Lam :: (VC c, VC b) => (b -> c) -> Expr (b -> c)
  App :: (VC a, VC b) => Expr (b -> a) -> Expr b -> Expr a

instance (Typeable a, Typeable b) => Show (a -> b) where
  show x = "(" ++ show (typeOf x) ++ ")"

deriving instance (Show a, Typeable a) => Show (Expr a)

data InterpreterType = Abstract | Concrete

type family Value (a :: *) (b :: InterpreterType) where
  Value a Concrete = a
  Value _ Abstract = Symbol

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)

newtype Exception a = ExceptDivByZero a deriving (Eq, Show)

type Eval t m =
  forall v. (VC v) => (Interpret t m) => Expr v -> m (Value v t)

type Combinator a = a -> a

type EvalCombinator t m = Combinator (Eval t m)

type Transformer t m = Combinator (EvalCombinator t m)

hoistOut1 :: Monad m => (a -> b) -> (a -> m b)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
hoistOut2 = (.) hoistOut1

hoistArgs :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do aa <- a; b >>= f aa

type Interpret t m = (InterpretI t m, InterpretB t m)

eval :: forall t m. EvalCombinator t m
eval ev expr = case expr of
  (Mul x y) -> hoistArgs (evalMul @t) (ev x) (ev y)
  (Div x y) -> hoistArgs (evalDiv @t) (ev x) (ev y)
  (And x y) -> hoistArgs (evalAnd @t) (ev x) (ev y)
  (LitB a) -> evalLitB @t a
  (LitI a) -> evalLitI @t a
  (Lam a) -> evalLam @t a
  (App (f :: Expr (b -> v)) (x :: Expr b)) ->
    hoistArgs (evalApp @t @m @v @b) (ev f) (ev x)

class (Monad m) => InterpretB (t :: InterpreterType) m where
  evalAnd :: (v ~ Value Bool t) => v -> v -> m v
  evalLitB :: (v ~ Value Bool t) => Bool -> m v

class (Monad m) => InterpretI (t :: InterpreterType) m where
  evalMul :: (v ~ Value Int t) => v -> v -> m v
  evalDiv :: (v ~ Value Int t) => v -> v -> m v
  evalLitI :: (v ~ Value Int t) => Int -> m v
  evalLam :: (v ~ Value (b -> a) t) => (b -> a) -> m v
  evalApp :: forall a b. Value (b -> a) t -> Value b t -> m (Value a t)

data SrcSpan = forall v. (Show v) => SrcSpan v

deriving instance Show SrcSpan

type MonadExec m = (MonadIO m, MonadReader SrcSpan m, MonadError (Exception SrcSpan) m, Monad m)

type MonadLint m = (MonadIO m, MonadReader SrcSpan m, MonadWriter [Exception SrcSpan] m, Monad m)

instance (MonadExec m) => InterpretI Concrete m where
  evalMul = hoistOut2 (*)
  evalDiv a 0 = ask >>= throwError . ExceptDivByZero
  evalDiv a b = hoistOut2 div a b
  evalLitI = hoistOut1 id
  evalLam = hoistOut1 id
  evalApp = hoistOut2 ($)

instance (MonadExec m) => InterpretB Concrete m where
  evalAnd = hoistOut2 (&&)
  evalLitB = hoistOut1 id

instance (MonadLint m) => InterpretI Abstract m where
  evalMul xx yy
    | xx == NotKnown || yy == NotKnown = return NotKnown
    | xx == Zero || yy == Zero = return Zero
    | otherwise = return NotZero
  evalDiv x y
    | y == Zero = ask >>= writer . (NotKnown,) . return . ExceptDivByZero
    | otherwise = return x
  evalLitI a = return $ if a == 0 then Zero else NotZero
  evalLam _ = return NotKnown
  evalApp _ _ = return NotKnown

instance (MonadLint m) => InterpretB Abstract m where
  evalAnd _ _ = return NotKnown
  evalLitB _ = return NotKnown

evAddSrc :: forall t m. (MonadReader SrcSpan m, Interpret t m) => Transformer t m
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace :: forall t m. (MonadIO m, Interpret t m) => Transformer t m
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval ::
  forall t m.
  (Interpret t m, MonadReader SrcSpan m, MonadIO m) =>
  Eval t m
extendedEval = fixEval @t $ evTrace @t $ evAddSrc @t $ eval @t

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)

type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (LitI 0)

fixEval :: forall m t. (Eval m t -> Eval m t) -> Eval m t
fixEval ev = ev (fixEval @m @t ev)

execEval :: (VC v) => Expr v -> IO ()
execEval expr =
  print
    =<< runExceptT
      ((`runReaderT` initSrc) $ extendedEval @Concrete @ValueExec expr)

execLint :: (VC v) => Expr v -> IO ()
execLint expr =
  print
    =<< runWriterT
      ((`runReaderT` initSrc) $ extendedEval @Abstract @ValueLint expr)

add1 :: Int -> Int
add1 = (+ 1)

expr :: Expr Int
expr = Mul (Div (LitI 1) (App (Lam add1) (LitI 1))) (Div (LitI 2) (LitI 0))

main :: IO ()
main = execEval expr
