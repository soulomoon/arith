{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.RWS
import           Control.Monad.Reader           ( ReaderT(runReaderT) )
import           Control.Monad.Writer

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

hoistArgs :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do aa <- a; b >>= f aa 

class (Monad m) => Interpret m v where
    eval :: (Expr -> m v) -> Expr -> m v
    eval ev (Mul x y) = hoistArgs evalMul (ev x) (ev y)
    eval ev (Div x y) = hoistArgs evalDiv (ev x) (ev y)
    eval ev (Lit a) = evalLit a

    evalMul :: v -> v -> m v
    evalDiv :: v -> v -> m v
    evalLit :: Int -> m v

newtype SrcSpan = SrcSpan Expr deriving (Show)
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

instance (MonadExec m) => Interpret m Int where
    evalMul = hoistOut2 (*)
    evalDiv a 0 = ask >>= throwError . ExceptDivByZero
    evalDiv a b = hoistOut2 div a b
    evalLit = hoistOut1 id

instance (MonadLint m) => Interpret m Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = return NotKnown
                  | xx == Zero || yy == Zero         = return Zero
                  | otherwise                        = return NotZero
    evalDiv x y = if y == Zero
        then ask >>= writer . (NotKnown, ) . return . ExceptDivByZero
        else return x
    evalLit a = return $ if a == 0 then Zero else NotZero

type Evaluator m v = Expr -> m v
type Combinator a = a -> a

evAddSrc :: (MonadReader SrcSpan m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evAddSrc ev ev' e = local (const $ SrcSpan e) $ ev ev' e

evTrace :: (MonadIO m, Interpret m v) => Combinator (Combinator (Evaluator m v))
evTrace ev ev' e = liftIO (print e) >> ev ev' e

extendedEval :: (Interpret m v, MonadReader SrcSpan m, MonadIO m) => Evaluator m v
extendedEval = fix $ evTrace $ evAddSrc eval

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

initSrc :: SrcSpan
initSrc = SrcSpan (Lit 0)

execEval :: Expr -> IO ()
execEval expr = print =<< runExceptT
    ((`runReaderT` initSrc) $ extendedEval @ValueExec @Int expr)

execLint :: Expr -> IO ()
execLint expr = print =<< runWriterT
    ((`runReaderT` initSrc) $ extendedEval @ValueLint @Symbol expr)

main :: IO ()
main = execLint $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 2) (Lit 0))

