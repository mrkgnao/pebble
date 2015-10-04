What's this?
------
A toy Haskell library for symbolic algebra, especially derivatives. I wrote it to learn to do a bunch of things, mainly.

I got a lot of the ideas and some of the code (I almost copied the `simplify` function from him) from [5outh's tutorial](http://5outh.blogspot.in/2013/05/symbolic-calculus-in-haskell.html), although I rewrote the code for derivatives on my own without looking at it.

I've implemented:

* Polynomials
* Trigonometric functions
* `exp` and `log`
* Four fundamental operations and exponentiation
* Symbolic derivatives
* Simplification of expressions, using:
    * A few trigonometric identities (stuff like `sin x sec x = tan x`) 
    * Basic algebraic identities (`a^0 = 0` etc.)

Examples
--------
x^x:

    λ> derivative (X :^ X)
    ((x ^ x) * (1.0 + (log x)))

sin (cos x):

    λ> derivative ((Apply ufsin (Apply ufcos X)))
    ((cos (cos x)) * (-(sin x)))

log (sin x):

    λ> derivative (Apply uflog (Apply ufsin X))
    (cot x)

Uh, why the hipster name?
-------------------------

What do you think ['calculus' originally meant](http://www.etymonline.com/index.php?term=calculus)?
