module Expr where

import Data.List

infixl 3 :%
infixl 4 :+, :-
infixl 5 :*, :/
infixr 6 :^

data UnaryFunc =
  UnaryFunc (Double -> Double) String

instance Eq UnaryFunc where
  (UnaryFunc f n1) == (UnaryFunc g n2) = (n1 == n2)

instance Show UnaryFunc where
  show (UnaryFunc f n) = show n

-- | a is the number type here.
data Expr
  = X
  | Const Double
  | (:%) Expr -- unary minus, I know it's ugly
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr :^ Expr
  | Apply UnaryFunc Expr
  deriving (Show, Eq)
