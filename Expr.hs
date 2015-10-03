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
  show (UnaryFunc f n) = n

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
  deriving Eq

instance Show Expr where
  show X = "x"
  show (Const x) = show x
  show ((:%) x) = "(-" ++ show x ++ ")"
  show (a :+ b) = wrapShow $ show a ++ " + " ++ show b
  show (a :- b) = wrapShow $ show a ++ " - " ++ show b
  show (a :* b) = wrapShow $ show a ++ " * " ++ show b
  show (a :/ b) = wrapShow $ show a ++ " / " ++ show b
  show (a :^ b) = wrapShow $ show a ++ " ^ " ++ show b
  show (Apply uf e) = "(" ++ show uf ++ " " ++ show e ++ ")"

isWrapped :: String -> Bool
isWrapped [] = False
isWrapped [_] = False
isWrapped [x,y] = (x == '(') && (y == ')')
isWrapped lst =
  noBrackets (init $ tail lst) && isWrapped [head lst,last lst]
  where noBrackets xs =
          0 == (length $ filter (== '(') xs) -- assuming matched parens

wrapShow :: String -> String
wrapShow str
  | isWrapped str = str
  | otherwise = "(" ++ str ++ ")"
