{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where
import           Control.Monad.Except
import           Control.Monad.Identity
import Control.Monad.RWS
    ( MonadReader(ask, local), MonadWriter(writer) )
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Control.Monad.Writer


data Expr a where
    Mul ::Expr Int -> Expr Int -> Expr Int
    Div ::Expr Int -> Expr Int -> Expr Int
    And ::Expr Bool -> Expr Bool -> Expr Bool
    LitI ::Int -> Expr Int
    LitB ::Bool -> Expr Bool
deriving instance (Show a) => Show (Expr a)

data InterpreterType = Abstract | Concrete
type family Value (a :: *) (b :: InterpreterType) where
    Value a Concrete = a
    Value _ Abstract = Symbol

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
newtype Exception a = ExceptDivByZero a deriving (Eq, Show)
type Evaluator m v t = Expr v -> m (Value v t)
type Combinator a = a -> a

hoistOut1 :: Monad m => (v -> v) -> (v -> m v)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (v -> v -> v) -> (v -> v -> m v)
hoistOut2 = (.) hoistOut1

hoistArgs :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do
    aa <- a
    b >>= f aa

type Interpret t m = (InterpretI t m, InterpretB t m)

eval :: forall t m v . (Interpret t m) => Combinator (Evaluator m v t)
eval ev (Mul x y  ) = hoistArgs (evalMul @t) (ev x) (ev y)
eval ev (Div x y  ) = hoistArgs (evalDiv @t) (ev x) (ev y)
eval ev (And x y  ) = hoistArgs (evalAnd @t) (ev x) (ev y)
eval ev (LitB a) = evalLitB @t a
eval ev (LitI  a) = evalLitI @t a

class (Monad m) => InterpretB (t :: InterpreterType) m  where
    evalAnd :: (v ~ Value Bool t) => v -> v -> m v
    evalLitB :: (v ~ Value Bool t) => Bool -> m v

class (Monad m) => InterpretI (t :: InterpreterType) m where
    evalMul :: (v ~ Value Int t) => v -> v -> m v
    evalDiv :: (v ~ Value Int t) => v -> v -> m v
    evalLitI :: (v ~ Value Int t) => Int -> m v

data SrcSpan = forall v . (Show v) => SrcSpan v
deriving instance Show SrcSpan

type MonadExec m
    = ( MonadIO m
      , MonadReader SrcSpan m
      , MonadError (Exception SrcSpan) m
      , Monad m
      )
type MonadLint m
    = ( MonadIO m
      , MonadReader SrcSpan m
      , MonadWriter [Exception SrcSpan] m
      , Monad m
      )

instance (MonadExec m) => InterpretI Concrete m  where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = ask >>= throwError . ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLitI = hoistOut1 id

instance (MonadExec m) => InterpretB Concrete m where
    evalAnd     = hoistOut2 (&&)
    evalLitB = hoistOut1 id

instance (MonadLint m) => InterpretI Abstract m where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
        else return x
    evalLitI a = return $ if a == 0 then Zero else NotZero

instance (MonadLint m) => InterpretB Abstract m where
    evalAnd _ _ = return NotKnown
    evalLitB _ = return NotKnown

evAddSrc
    :: forall t m v
     . (MonadReader SrcSpan m, Interpret t m, Show v)
    => Combinator (Combinator (Evaluator m v t))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace
    :: forall t m v
     . (MonadIO m, Interpret t m, Show v)
    => Combinator (Combinator (Evaluator m v t))
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval
    :: forall t m v
     . (Interpret t m, MonadReader SrcSpan m, MonadIO m, Show v)
    => Evaluator m v t
extendedEval = fix $ (evTrace @t) $ (evAddSrc @t) (eval @t)

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (LitI 0)


execEval :: (Show v) => Expr v -> IO ()
execEval expr = print =<< runExceptT
    ((`runReaderT` initSrc) $ extendedEval @Concrete @ValueExec expr)

execLint :: (Show v) => Expr v -> IO ()
execLint expr = print =<< runWriterT
    ((`runReaderT` initSrc) $ extendedEval @Abstract @ValueLint expr)

expr :: Expr Int
expr = Mul (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))

main :: IO ()
main = execEval expr

