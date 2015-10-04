module Main where

import Expr
import Derivative
import Simplify
import Functions

expr :: Expr
expr = (Apply ufsin X :/ (Apply ufcos X))

main :: IO ()
main = print (fullSimplify $ expr)
