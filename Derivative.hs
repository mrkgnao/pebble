module Derivative where

import Expr
import Functions
import Simplify

deriv :: Expr -> Expr
deriv (Const _) = Const 0

deriv (X) = Const 1

deriv (a :+ b) = (deriv a) :+ (deriv b)

deriv (a :- b) = (deriv a) :- (deriv b)

deriv (a :* b) = a :* (deriv b) :+ (deriv a) :* b

deriv (a :/ b) = (b :* (deriv a) :- a :* (deriv b)) :/ (b :* b)

deriv (a :^ b) = (a :^ b) :* (deriv (b :* (Apply uflog a)))

deriv ((:%) a) = (Const (-1)) :* (deriv a)

deriv (Apply u e) = (u' e) :* (deriv e)
  where (UnaryFunc f n) = u
        u' =
          case lookup u derivsList of
            (Just g) -> g
            Nothing -> error $ "I don't know how to differentiate " ++ n

derivative :: Expr -> Expr
derivative = fullSimplify . deriv
