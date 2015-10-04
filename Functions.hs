module Functions where

import Data.Maybe

import Expr

csc x = 1 / sin x
sec x = 1 / cos x
cot x = cos x / sin x

ufsin = UnaryFunc sin "sin"
ufcos = UnaryFunc cos "cos"
uftan = UnaryFunc tan "tan"
ufsec = UnaryFunc sec "sec"
ufcsc = UnaryFunc csc "csc"
ufcot = UnaryFunc cot "cot"
uflog = UnaryFunc log "log"
ufexp = UnaryFunc exp "exp"

ufsin' = Apply ufcos
ufcos' = (:%) . Apply ufsin
uftan' = (:^ (Const 2)) . Apply ufsec
ufcot' = (:* (Const (-1))) . (:^ (Const 2)) . Apply ufcsc
ufsec' e = ((Apply ufsec e) :* (Apply uftan e))
ufcsc' e = ((Const (-1)) :* ((Apply ufsec e) :* (Apply uftan e)))
ufexp' = Apply ufexp
ufrecip' = (Const 1 :/)

derivsList :: [(UnaryFunc, Expr -> Expr)]
derivsList =
  [(ufsin,ufsin')
  ,(ufcos,ufcos')
  ,(uftan,uftan')
  ,(ufcot,ufcot')
  ,(ufsec,ufsec')
  ,(ufcsc,ufcsc')
  ,(uflog,ufrecip')
  ,(ufexp,ufexp')]

inv :: UnaryFunc -> UnaryFunc
inv func
  | isJust inverse = func'
  where inverse = lookup func lst''
        (Just func') = inverse

invsList :: [(UnaryFunc, Expr -> Expr)]
invsList = map (\(x,y) -> (x,Apply y)) lst''
lst = [(ufsin,ufcsc),(ufcos,ufsec),(uftan,ufcot)]
lst' = map (uncurry $ flip (,)) lst
lst'' = lst ++ lst'

prodList :: [((UnaryFunc, UnaryFunc), Expr -> Expr)]
prodList =
  map (\(x,y,z) -> ((x,y),z)) $ prods ++ flipProds ++ consts
  where prods =
          [(ufsin,ufsec,Apply uftan),(ufcos,ufcsc,Apply ufcot)]
        flipProds =
          map (\(x,y,z) -> (y,x,z)) prods
        consts =
          map (\(x,y) -> (x,y,const (Const 1))) lst''

quotList :: [((UnaryFunc, UnaryFunc), Expr -> Expr)]
quotList = [((ufsin, ufcos), Apply uftan)]
