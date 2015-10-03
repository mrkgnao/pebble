module Functions where

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
