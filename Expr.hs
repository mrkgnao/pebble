module Expr where

import Data.List

infixl 3 :--
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
  | (:--) Expr
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
  | Expr :^ Expr
  | Apply UnaryFunc Expr
  deriving (Show, Eq)

mkConstFunc :: Double -> UnaryFunc
mkConstFunc x = UnaryFunc (const x) (show x)

ufsin = UnaryFunc sin "sin"
ufcos = UnaryFunc cos "cos"

derivsList :: [(UnaryFunc, UnaryFunc)]
derivsList = [(ufsin, ufcos)]

deriv :: Expr -> Expr
deriv (Const _) = Const 0

deriv (X) = Const 1

deriv (a :+ b) = (deriv a) :+ (deriv b)

deriv (a :- b) = (deriv a) :- (deriv b)

deriv (a :* b) = a :* (deriv b) :+ (deriv a) :* b

deriv (a :/ b) = (b :* (deriv a) :- a :* (deriv b)) :/ (b :* b)

deriv ((:--) a) = (Const (-1)) :* (deriv a)

deriv (Apply u e)
  | u == ufcos = ((:--) $ Apply ufsin e) :* (deriv e)
  | otherwise = (Apply u' e) :* (deriv e)
  where (UnaryFunc f n) = u
        u' =
          case lookup u derivsList of
            (Just g) -> g
            Nothing -> error $ "I don't know how to differentiate " ++ n
