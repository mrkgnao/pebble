module Simplify where

import Expr
import Functions

-- | Cleans up nonsense like X :^ X :* (X :* ((Const 1.0 :/ X) :* Const 1.0) :+
-- | Const 1.0 :* Apply "log" X) into (hopefully) nicer expressions like
-- | X :^ X ((Const 1.0) :+ Apply "log" X),

simplify :: Expr -> Expr
simplify (Const a :+ Const b) = Const (a + b)
simplify (a :+ Const 0) = simplify a
simplify (Const 0 :+ a) = simplify a

simplify (Const a :* Const b) = Const (a * b)
simplify (a :* Const 1) = simplify a
simplify (Const 1 :* a) = simplify a
simplify (a :* Const 0) = Const 0
simplify (Const 0 :* a) = Const 0

simplify (Const a :^ Const b) = Const (a ** b)
simplify (a :^ Const 1) = simplify a
simplify (a :^ Const 0) = Const 1
simplify ((c :^ Const b) :^ Const a) =
  c :^ (Const (a * b))

-- | Multiplication

-- m * (n * f) = (m * n) * f
simplify (Const a :* (Const b :* expr)) =
  (Const $ a * b) :* (simplify expr)

-- mfn = mnf
simplify (Const a :* expr :* Const b) =
  (Const $ a * b) :* (simplify expr)

-- fmn = mnf
simplify (expr :* Const a :* Const b) =
  (Const $ a * b) :* (simplify expr)

-- m(f+g) = mf+mg
simplify (Const a :* (b :+ c)) =
  (Const a :* (simplify b)) :+ (Const a :* (simplify c))

simplify (Const 0 :/ a) = Const 0
simplify (Const a :/ Const 0) =
  error "Division by zero!"
simplify (Const a :/ Const b) = Const (a / b)
simplify (a :/ Const 1) = simplify a
simplify (a :/ b) | a == b = Const 1
simplify (a :* (Const b :/ c)) = Const b :* simplify (a :/ c)

simplify (a :/ b) = (simplify a) :/ (simplify b)
simplify (a :^ b) = (simplify a) :^ (simplify b)
simplify (a :* b) = (simplify a) :* (simplify b)
simplify (a :+ b) = (simplify a) :+ (simplify b)
simplify x = x

fullSimplify expr =
  fullSimplify' expr
                (Const 0) -- placeholder
  where fullSimplify' cur last
          | cur == last = cur
          | otherwise =
            let cur' = simplify cur
            in fullSimplify' cur' cur
