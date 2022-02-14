{-# LANGUAGE TypeApplications #-}

module Main where
import           Control.Arrow                  ( (>>>) )

data Expr =
    Mul Expr Expr
    | Div Expr Expr
    | Lit Int

data Symbol = Zero | NotZero | NotKnown deriving (Show, Eq)

class Interpret v where
    eval :: Expr -> v
    eval (Mul x y) = evalMul (eval x) (eval y)
    eval (Div x y) = evalDiv (eval x) (eval y)
    eval (Lit a) = evalLit a

    evalMul :: v -> v -> v
    evalDiv :: v -> v -> v
    evalLit :: Int -> v

instance Interpret Int where
    evalMul = (*)
    evalDiv = div
    evalLit = id


instance Interpret Symbol where
    evalMul xx yy | xx == NotKnown || yy == NotKnown = NotKnown
                  | xx == Zero || yy == Zero         = Zero
                  | otherwise                        = NotZero
    evalDiv xx yy = if yy == Zero || yy == NotKnown then NotKnown else xx
    evalLit a = if a == 0 then Zero else NotZero

execEval :: Expr -> IO ()
execEval = print . eval @Int
execLint :: Expr -> IO ()
execLint = print . eval @Symbol

main :: IO ()
main = do
    print "hello"
    execEval $ Lit 1
    execEval $ Mul (Lit 1) (Lit 2)
    execLint $ Mul (Lit 1) (Lit 2)

    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Div (Lit 1) (Lit 0)
    execLint $ Mul (Div (Lit 1) (Lit 0)) (Div (Lit 1) (Lit 0))
    print "End"

