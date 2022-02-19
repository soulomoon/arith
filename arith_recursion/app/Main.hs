{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow ((>>>))

data ExprF a = MulF a a | DivF a a | LitF Int deriving (Functor)

newtype Fix f = Fix {unFix :: f (Fix f)}

type Expr = Fix ExprF

type Algebra f a = f a -> a

pattern Mul a b = Fix (MulF a b)

pattern Div a b = Fix (DivF a b)

pattern Lit a = Fix (LitF a)

{-# COMPLETE Mul, Div, Lit #-}

cata :: (Functor f) => Algebra f a -> Fix f -> a
cata f = unFix >>> fmap (cata f) >>> f

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)

eval :: Interpret v => Expr -> v
eval = cata evalF

evalF :: Interpret v => Algebra ExprF v
evalF (MulF x y) = evalMul x y
evalF (DivF x y) = evalDiv x y
evalF (LitF a) = evalLit a

class Interpret v where
  evalMul :: v -> v -> v
  evalDiv :: v -> v -> v
  evalLit :: Int -> v

instance Interpret Int where
  evalMul = (*)
  evalDiv = div
  evalLit = id

instance Interpret Symbol where
  evalMul x y
    | x == NotKnown || y == NotKnown = NotKnown
    | x == Zero || y == Zero = Zero
    | otherwise = NotZero
  evalDiv x y = if y == Zero || y == NotKnown then NotKnown else x
  evalLit a = if a == 0 then Zero else NotZero

execEval :: Expr -> IO ()
execEval = print . eval @Int

execLint :: Expr -> IO ()
execLint = print . eval @Symbol

main :: IO ()
main = do
  print "hello"
  execLint $ Lit 1
  execLint $ Mul (Lit 1) (Lit 2)
  execLint $ Mul (Lit 1) (Lit 2)

  execLint $ Div (Lit 1) (Lit 0)
  execLint $ Div (Lit 1) (Lit 0)
  execLint $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 1) (Lit 0))
  print "End"
