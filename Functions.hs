module Functions where

import Expr

ufsin = UnaryFunc sin "sin"
ufcos = UnaryFunc cos "cos"
uflog = UnaryFunc log "log"
ufexp = UnaryFunc exp "exp"

ufsin' = Apply ufcos
ufcos' = (:%) . Apply ufsin
ufexp' = Apply ufexp
ufrecip' = (Const 1 :/)

derivsList :: [(UnaryFunc, Expr -> Expr)]
derivsList =
  [(ufsin,ufsin'),(ufcos,ufcos'),(uflog,ufrecip'),(ufexp,ufexp')]
