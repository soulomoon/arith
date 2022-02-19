-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where



data InterpreterType = Abstract | Concrete
data SymbolI = Zero | NotZero | NotKnown deriving (Show, Eq)
data SymbolB = TrueOrFalse deriving (Show, Eq)

type family Value (a :: *) (b :: InterpreterType) = r | r -> a b where
  Value Bool Abstract = SymbolB
  Value Int Abstract = SymbolI
  Value Int Concrete = Int
  Value Bool Concrete = Bool
  Value (a -> b) t = Value a t -> Value b t


-- data ExprF a where
--   LamF :: Value a -> ExprF (Value a)

-- lamf :: Value a -> ExprF (Value a)
-- lamf a = LamF a


main :: IO ()
main =
  print 1

-- execEval (LitI 1)
-- execEval (Mul (LitI 2) (LitI 4))