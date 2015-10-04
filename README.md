pebble
------

A toy Haskell library for symbolic algebra, especially derivatives. Mainly educational. 

`λ> deriv $ (Apply ufsin X) :/ (Apply ufcos X)
((((cos x) * (cos x)) - ((sin x) * (-(sin x)))) / ((cos x) * (cos x)))
λ> deriv $ (Apply ufsin X) :/ (Apply ufcos X)
((((cos x) * (cos x)) - ((sin x) * (-(sin x)))) / ((cos x) ^ 2.0))
λ> derivative $ (Apply ufsin X) :/ (Apply ufcos X)
((((cos x) * (cos x)) - ((sin x) * (-(sin x)))) / ((cos x) ^ 2.0)
`
