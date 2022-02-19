-- {-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Functor.Compose
import Data.Typeable (typeOf)
import Control.Category ((>>>))

data ExprF (t :: InterpreterType) (f :: * -> *) :: * -> * where
  MulF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  AndF :: (v ~ Value Bool t) => f v -> f v -> ExprF t f v
  DivF :: (v ~ Value Int t) => f v -> f v -> ExprF t f v
  LitIF :: Int -> ExprF t f (Value Int t)
  LitBF :: Bool -> ExprF t f (Value Bool t)
  LamF :: Value (b -> c) t -> ExprF t f (Value (b -> c) t)
  AppF :: (Show (Value b t)) => f (Value (b -> a) t) -> f (Value b t) -> ExprF t f (Value a t)

instance (Show (f a), Show a) => Show (ExprF t f a) where
  showsPrec d =
    \case
      (AndF a b) -> showsCon1 "And" (show a ++ show b)
      (MulF a b) -> showsCon1 "Mul" (show a ++ show b)
      (DivF a b) -> showsCon1 "Div" (show a ++ show b)
      (LamF a ) -> showsCon1 "Lam" (show a)
      (LitIF a) -> showsCon1 "LitI" (show a)
      (LitBF a) -> showsCon1 "LitB" (show a)
      (AppF a b) -> showsCon1 "App" ""
   where
    showsCon1 :: String -> String -> String -> String
    showsCon1 con a =
      showParen True $ showString (con <> " ") . showString a
instance (Show a, Show (f (Fix f) a)) => Show (Fix f a) where show (Fix x) = show x
instance Show (a -> b) where show x = "(" ++ "" ++ ")"

-- type Nat f g h = (forall a . (Show a => f a -> g a) -> (forall a. h f a -> h g a)
type f ~> g = forall a .(Show a) => f a -> g a
class Functor2 (h :: (* -> *) -> * -> *) where
  map2:: f ~> g -> h f ~> h g
instance Functor2 (ExprF t) where
  map2 f (LitBF i) = LitBF i
  map2 f (AndF a b) = AndF (f a) (f b)
  map2 f (LitIF i) = LitIF i
  map2 f (LamF i)  = LamF i
  map2 f (MulF a b) = MulF (f a) (f b)
  map2 f (DivF a b) = DivF (f a) (f b)
  map2 f (AppF a b) = AppF (f a) (f b)

-- catamorphism with transformer
cata :: forall h f. (Functor2 h) => Transform f h -> (h f ~> f) -> (Fix h ~> f)
cata f alg = f (alg . map2 (cata f alg) . unFix)

evalAlg :: forall t m. (Interpret t m) => ExprF t m ~> m
evalAlg expr = case expr of
  (MulF x y) -> hoistArgs (evalMul @t) x y
  (LitIF a) -> evalLitI @t a
  (DivF x y) -> hoistArgs (evalDiv @t) x y
  (AndF x y) -> hoistArgs (evalAnd @t) x y
  (LitBF a) -> evalLitB @t a
  (LamF a) -> evalLam @t a
  (AppF f x) -> hoistArgs (evalApp @t) f x

newtype Fix f a = Fix { unFix :: f (Fix f) a }
type Expr t = Fix (ExprF t)

data InterpreterType = Abstract | Concrete
data SymbolI = Zero | NotZero | NotKnown deriving (Show, Eq)
data SymbolB = TrueOrFalse deriving (Show, Eq)

type family Value (a :: *) (b :: InterpreterType) = r | r -> a b where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value Int Concrete = Int
  Value Bool Concrete = Bool
  Value (a -> b) t = Value a t -> Value b t

newtype Exception a = ExceptDivByZero a deriving (Eq, Show)

hoistOut1 :: Monad m => (a -> b) -> (a -> m b)
hoistOut1 = (.) return

hoistOut2 :: Monad m => (a -> b -> c) -> (a -> b -> m c)
hoistOut2 = (.) hoistOut1

hoistArgs :: Monad m => (a -> b -> m c) -> (m a -> m b -> m c)
hoistArgs f a b = do aa <- a; b >>= f aa

type Interpret t m = (InterpretI t m, InterpretB t m, InterpretL t m)

class (Monad m) => InterpretB (t :: InterpreterType) m where
  evalAnd :: (v ~ Value Bool t) => v -> v -> m v
  evalLitB :: (v ~ Value Bool t) => Bool -> m v

class (Monad m) => InterpretI (t :: InterpreterType) m where
  evalMul :: (v ~ Value Int t) => v -> v -> m v
  evalDiv :: (v ~ Value Int t) => v -> v -> m v
  evalLitI :: (v ~ Value Int t) => Int -> m v

class (Monad m) => InterpretL (t :: InterpreterType) m where
  evalLam :: (Value b t -> Value a t) -> m (Value (b -> a) t)
  evalApp :: Value (b -> a) t -> Value b t -> m (Value a t)

newtype SrcSpan = SrcSpan String deriving (Eq, Show)
type MonadExec m s = (MonadIO m, MonadReader s m, MonadError (Exception s) m, Monad m)
type MonadLint m s = (MonadIO m, MonadReader s m, MonadWriter [Exception s] m, Monad m)

instance (MonadExec m s) => InterpretI Concrete m where
  evalMul = hoistOut2 (*)
  evalDiv a 0 = ask >>= throwError . ExceptDivByZero
  evalDiv a b = hoistOut2 div a b
  evalLitI = hoistOut1 id

instance (MonadExec m s) => InterpretL Concrete m where
  evalLam = hoistOut1 id
  evalApp = hoistOut2 ($)

instance (MonadExec m s) => InterpretB Concrete m where
  evalAnd = hoistOut2 (&&)
  evalLitB = hoistOut1 id

instance (MonadLint m s) => InterpretI Abstract m where
  evalMul xx yy
    | xx == NotKnown || yy == NotKnown = return NotKnown
    | xx == Zero || yy == Zero = return Zero
    | otherwise = return NotZero
  evalDiv x y
    | y == Zero = ask >>= writer . (NotKnown,) . return . ExceptDivByZero
    | otherwise = return x
  evalLitI a = return $ if a == 0 then Zero else NotZero

instance (MonadLint m s) => InterpretL Abstract m where
  evalLam = hoistOut1 id
  evalApp ap b = return $ ap b

instance (MonadLint m s) => InterpretB Abstract m where
  evalAnd _ _ = return TrueOrFalse
  evalLitB _ = return TrueOrFalse

type ValueExec = ReaderT SrcSpan (ExceptT (Exception SrcSpan) IO)
type ValueLint = ReaderT SrcSpan (WriterT [Exception SrcSpan] IO)

-- to stabilize the a, fix on a.
type Transform f h = forall a.(Show a) => (Fix h a -> f a) -> (Fix h a -> f a)
evAddSrc :: (MonadIO m, Interpret t m, MonadReader SrcSpan m) => (Transform m (ExprF t))
evAddSrc f e = local (const $ SrcSpan (show e)) $ f e
evTrace :: (MonadIO m, Interpret t m, MonadReader SrcSpan m) => (Transform m (ExprF t))
evTrace f e = liftIO (print e) >> f e

eval :: forall t m a. (Interpret t m, MonadIO m, MonadReader SrcSpan m) => Expr t ~> m
eval = cata (evAddSrc . evTrace) (evalAlg @t @m)

execEval :: Show a => Expr 'Concrete a -> IO ()
execEval expr = runExceptT ((`runReaderT` SrcSpan "") $ eval @Concrete @ValueExec expr) >>= print

execLint :: Show a => Expr 'Abstract a -> IO ()
execLint expr = runWriterT ((`runReaderT` SrcSpan "") $ eval @Abstract @ValueLint expr) >>= print

add1 :: Int -> Int
add1 = (+ 10)

double :: (Int -> Int) -> (Int -> Int)
double f = f . f

expr0 :: Fix (ExprF 'Concrete) Int
expr0 = App (App (Lam double) (Lam add1)) (LitI 1)

-- expr :: Expr t Int
expr :: Fix (ExprF 'Concrete) Int
expr = Mul (Div (LitI 1) (App (Lam add1) (LitI 1))) (Div (LitI 2) (LitI 0))

main :: IO ()
main = do
  execEval (Mul (Div (LitI 2) (LitI 0)) (Div (LitI 4) (LitI 0))) 
  execLint (Mul (Div (LitI 2) (LitI 0)) (Div (LitI 4) (LitI 0))) 
  execEval expr0 >>= print
  print 1

pattern Mul a b = Fix (MulF a b)

pattern LitI a = Fix (LitIF a)
pattern LitB a = Fix (LitBF a)

pattern Div a b = Fix (DivF a b)
pattern Lam a  = Fix (LamF a)
pattern App a  b = Fix (AppF a b)
pattern And a  b = Fix (AndF a b)
{-# COMPLETE Mul, LitI, Div, Lam, App, LitB, And #-}

