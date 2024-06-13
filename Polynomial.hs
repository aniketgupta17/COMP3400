module Polynomial (expand, simplify, Polynomial(..)) where

{--

*DO NOT* import any modules.
*You are allowed to use anything available in Prelude and any syntax features* 

You may remove any of the comments if you like.

Below is a data type for representing univariate polynomials from NN[x].  That
is, polynomials in x with NATURAL (non-negative integer) coefficients.

Arithmetic on NN[x] is easier than it is on ZZ[x] (polynomials with integer
coefficients) becuase you do not have to worry about terms cancelling.

type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always positive.
data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial

For instance:
1/  3x^2 + 2x + 1
is encoded by
  Add (Mono 3 2) $ Add (Mono 2 1) $ (Mono 1 0)

2/  (2x + 1) * (x^2 + 2)
is encoded by
  Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))

============
PRECONDITION
============

1.  Assume monomials always have degree >= 0 and coefficients >0.

====
TASK
====

Write TWO functions
  expand :: Polynomial -> Polynomial
  simplify :: Polynomial -> Polynomial

============================
expand :: Polynomial -> Polynomial
============================

This function takes a polynomial and returns an equivalent polynomimal where
multiplications have been removed.

For example
1/  f = (2x + 1) * (x^2 + 2) = 2x^3 + 4x + x^2 + 2
2/  g = (x + 1) * (x + 1) = x^2 + x + x + 1
NOTE, *do not* combine like terms (i.e. simplify) --- just remove Mul without
changing the polynominal.

=======
EXAMPLE
=======
NOTE your solution does not have to look identical to the following examples.
We will be conducting PROPERTY TESTING of you code.  That is, we will confirm
your output is EQUAL to the input and DOES NOT CONTAIN the Mul constructor.

> f = Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))
> expand f
Add (Add (Mono 2 3) (Mono 4 1)) (Add (Mono 1 2) (Mono 2 0))

> g = Mul (Add (Mono 1 1) (Mono 1 0)) (Add (Mono 1 1) (Mono 1 0))
> expand g
Add (Add (Mono 1 2) (Mono 1 1)) (Add (Mono 1 1) (Mono 1 0))

==============================
simplify :: Polynomial -> Polynomial
==============================

Every polynomial can be written in SIMPLIFIED FORM like
   a{n}*x^{n} + a{n-1}*x^{n-1} + ... + a{1}x + a{0}
In particular, the monomials are given in DESCENDING degree order.

For example,
1/  f = (2x + 1) * (x^2 + 2)
simplifies to
    2x^3 + x^2 + 4x + 2

2/  g = x^2 + x + x + 1
simplifies to
    x^2 + 2x + 1

Notice like terms 'x' and 'x' are now combined to '2x'.

=======
EXAMPLE
=======
NOTE simplify returns a CANONICAL FORM and therefore your answers MUST BE
IDENTICAL to the output here.

> f = Mul (Add (Mono 2 1) (Mono 1 0)) (Add (Mono 1 2) (Mono 2 0))
> simplify f
Add (Mono 2 3) (Add (Mono 1 2) (Add (Mono 4 1) (Mono 2 0)))

> g = Mul (Add (Mono 1 1) (Mono 1 0)) (Add (Mono 1 1) (Mono 1 0))
> simplify g
Add (Mono 1 2) (Add (Mono 2 1) (Mono 1 0))

--}


-- Define custom types for degree and coefficient
type Deg = Integer   -- precondition: always nonnegative.
type Coeff = Integer -- precondition: always positive.

-- Define the polynomial data type with constructors for monomial, addition, and multiplication
data Polynomial = Mono Coeff Deg | Add Polynomial Polynomial | Mul Polynomial Polynomial
    deriving Show

-- Function to eliminate multiplication in the polynomial
expand :: Polynomial -> Polynomial
-- If the polynomial is a monomial, return it as is
expand (Mono c d) = Mono c d
-- If the polynomial is an addition, recursively expand each sub-polynomial
expand (Add p1 p2) = Add (expand p1) (expand p2)
-- If the polynomial is a multiplication, recursively expand each sub-polynomial and combine terms
expand (Mul p1 p2) = expandMul (expand p1) (expand p2)

-- Helper function to expand multiplication
expandMul :: Polynomial -> Polynomial -> Polynomial
-- If both polynomials are monomials, multiply their coefficients and add their degrees
expandMul (Mono c1 d1) (Mono c2 d2) = Mono (c1 * c2) (d1 + d2)
-- If the first polynomial is a monomial and the second is an addition, distribute the first monomial over each term of the addition
expandMul (Mono c d) (Add p1 p2) = Add (expandMul (Mono c d) p1) (expandMul (Mono c d) p2)
-- If both polynomials are additions, distribute each term of the first addition over each term of the second addition
expandMul (Add p1 p2) p = Add (expandMul p1 p) (expandMul p2 p)

-- Function to simplify the polynomial in descending order of degree
simplify :: Polynomial -> Polynomial
-- Simplify the expanded polynomial
simplify = simplifyHelper . expand

-- Helper function to simplify the expanded polynomial
simplifyHelper :: Polynomial -> Polynomial
-- If the polynomial is a monomial, return it as is
simplifyHelper (Mono c d) = Mono c d
-- If the polynomial is an addition, simplify each sub-polynomial
simplifyHelper (Add p1 p2) = simplifyAdd (simplifyHelper p1) (simplifyHelper p2)

-- Helper function to simplify addition of polynomials
simplifyAdd :: Polynomial -> Polynomial -> Polynomial
-- If both polynomials are monomials with the same degree, combine their coefficients
simplifyAdd (Mono c1 d1) (Mono c2 d2)
    | d1 == d2  = Mono (c1 + c2) d1
    -- If the degrees are different, keep them separate
    | otherwise = Add (Mono c1 d1) (Mono c2 d2)
-- If the first polynomial is a monomial and the second is an addition, combine like terms
simplifyAdd (Mono c d) (Add (Mono c1 d1) p2)
    -- If the degrees match, combine coefficients
    | d == d1   = simplifyAdd (Mono (c + c1) d) (simplifyHelper p2)
    -- If the degree of the monomial is greater, keep them separate
    | d > d1    = Add (Mono c d) (simplifyAdd (Mono c1 d1) (simplifyHelper p2))
    -- If the degree of the monomial is lesser, keep them separate
    | otherwise = Add (Mono c1 d1) (simplifyAdd (Mono c d) (simplifyHelper p2))
-- If both polynomials are additions, simplify each sub-polynomial
simplifyAdd (Add p1 p2) p = simplifyAdd (simplifyHelper p1) (simplifyAdd (simplifyHelper p2) p)

