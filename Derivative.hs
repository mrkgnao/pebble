module Derivative where

import Expr
import Functions
import Simplify

deriv :: Expr -> Expr
deriv (Const _) = Const 0

deriv (X) = Const 1

deriv (a :+ b) = fullSimplify $ (deriv a) :+ (deriv b)

deriv (a :- b) = fullSimplify $ (deriv a) :- (deriv b)

deriv (a :* b) = fullSimplify $ a :* (deriv b) :+ (deriv a) :* b

deriv (a :/ b) = fullSimplify $ (b :* (deriv a) :- a :* (deriv b)) :/ (b :* b)

deriv (a :^ b) = fullSimplify $ (a :^ b) :* (deriv (b :* (Apply uflog a)))

deriv ((:%) a) = (Const (-1)) :* (fullSimplify $ deriv a)

deriv (Apply u e) = fullSimplify $ (u' e) :* (deriv e)
  where (UnaryFunc f n) = u
        u' =
          case lookup u derivsList of
            (Just g) -> g
            Nothing -> error $ "I don't know how to differentiate " ++ n


-- | Calculate the derivative of a function and simplify it.
derivative :: Expr -> Expr
derivative = fullSimplify . deriv

-- | Calculate the nth derivative of a function.
nderiv :: Int -> Expr -> Expr
nderiv n = foldr1 (.) (replicate n derivative)
