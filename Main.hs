module Main where

import Expr
import Derivative
import Simplify
import Functions

expr :: Expr
expr = Apply uflog (Apply ufsin X)

main :: IO ()
main = print (simplify $ deriv expr)
