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


data InterpreterType = Abstract | Concrete
data Expr (t :: InterpreterType) a where
    Div ::Expr t (Value Int t)-> Expr t (Value Int t) -> Expr t (Value Int t)
    And ::Expr t (Value Bool t) -> Expr t (Value Bool t) -> Expr t (Value Bool t)
    LitI ::Int -> Expr t (Value Int t)
    LitB ::Bool -> Expr t (Value Bool t)

deriving instance (Show a) => Show (Expr t a)

type family Value (a :: *) (b :: InterpreterType) where
    Value a Concrete = a
    Value _ Abstract = Symbol

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)
newtype Exception a = ExceptDivByZero a deriving (Eq, Show)
type Evaluator m v t = Expr t v -> m v
type Combinator a = a -> a

hoistOut1 :: Monad m => (v -> v) -> (v -> m v)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (v -> v -> v) -> (v -> v -> m v)
hoistOut2 = (.) hoistOut1

hoistArgs :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do
    aa <- a
    b >>= f aa

type Interpret m v = (InterpretI m v, InterpretB m v)

eval :: forall t m v . (Interpret m v) => Combinator (Evaluator m v t)
eval ev (Div x y  ) = hoistArgs evalDiv  (ev x) (ev y)
eval ev (And x y  ) = hoistArgs evalAnd  (ev x) (ev y)
eval ev (LitB a) = evalLitB a
eval ev (LitI  a) = evalLitI a

class (Monad m) => InterpretB m v where
    evalAnd ::  v -> v -> m v
    evalLitB :: Bool -> m v

class (Monad m) => InterpretI m v where
    evalMul ::  v -> v -> m v
    evalDiv ::  v -> v -> m v
    evalLitI :: Int -> m v

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

instance (MonadExec m) => InterpretI m Int where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = ask >>= throwError . ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLitI = hoistOut1 id

instance (MonadExec m) => InterpretB m Bool where
    evalAnd     = hoistOut2 (&&)
    evalLitB = hoistOut1 id

instance (MonadLint m) => InterpretI m Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
        else return x
    evalLitI a = return $ if a == 0 then Zero else NotZero

instance (MonadLint m) => InterpretB m Symbol where
    evalAnd _ _ = return NotKnown
    evalLitB _ = return NotKnown

evAddSrc
    :: forall t m v
     . (MonadReader SrcSpan m, Interpret m v, Show v)
    => Combinator (Combinator (Evaluator m v t))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace
    :: forall t m v
     . (MonadIO m, Interpret m v, Show v)
    => Combinator (Combinator (Evaluator m v t))
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval
    :: forall t m v
     . (Interpret m v, MonadReader SrcSpan m, MonadIO m, Show v)
    => Evaluator m v t
extendedEval = fix $ (evTrace @t) $ (evAddSrc @t) (eval @t)

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (LitI @Concrete 0)


execEval :: forall v. (Show v, Interpret ValueExec v) => Expr Concrete v -> IO ()
execEval expr = print =<< runExceptT
    ((`runReaderT` initSrc) $ extendedEval @Concrete @ValueExec expr)

-- execLint :: (Show v) => Expr Abstract v -> IO ()
-- execLint expr = print =<< runWriterT
--     ((`runReaderT` initSrc) $ extendedEval @Abstract @ValueLint expr)

-- expr :: Expr Abstract Symbol
expr :: Expr t (Value Int t)
expr = Div (Div (LitI 1) (LitI 0)) (Div (LitI 2) (LitI 0))

main :: IO ()
main = execEval expr

