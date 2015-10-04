# What's this?

A toy Haskell library for symbolic algebra, especially derivatives. I wrote it to learn to do a bunch of things, mainly.

I got a lot of the ideas and some of the code (I almost copied the `simplify` function from him) from [5outh's tutorial](http://kovach.me/posts/2013-05-01-symbolic-calculus.html), although I rewrote the code for derivatives on my own without looking at his.

I've implemented:

* Polynomials
* Trigonometric functions
* `exp` and `log`
* Four fundamental operations and exponentiation
* Symbolic derivatives
* Simplification of expressions, using:
    * A few trigonometric identities (stuff like `sin x sec x = tan x`) 
    * Basic algebraic identities (`a^0 = 1` etc.)

To use it, clone the repo into a folder and run `Main.hs` in `ghci`:

    $ git clone https://github.com/mrkgnao/pebble.git
    $ cd pebble
    $ ghci Main.hs

# Syntax (for now)

* `:+`, `:-`, `:*`, `:/` and `:^` are what you think they are
* The same goes for `X`
* Unary functions are given their Prelude names with an `uf` prefix, and you apply them to an expression with `(Apply uffunc expr)`, where `uffunc` is something like `ufsin`.
* Operators have precedence, so you don't have to wrap everything in parens

## Syntax examples

* `X :^ X` is `x^x`
* `Apply ufsin (Apply uflog X)` is `sin (log x)`
* `(Apply ufsin (2 :+ X)) :* (Apply ufcot X)` is `sin (2+x) cot x`

## Differentiation examples

The `derivative` function computes the derivative of an expression, simplifies it as best as it can and pretty-prints it.

The bane of every eleventh-grader:

    λ> derivative (X :^ X)
    ((x ^ x) * (1.0 + (log x)))

A composition of two functions:

    λ> derivative ((Apply ufsin (Apply ufcos X)))
    ((cos (cos x)) * (-(sin x)))

This pushes the, uh, Synergized Algebraic Simplification Engine™ to its limit:

    λ> derivative (Apply uflog (Apply ufsin X))
    (cot x)

Uh, why the hipster name?
-------------------------

What do you think ['calculus' originally meant](http://www.etymonline.com/index.php?term=calculus)?
