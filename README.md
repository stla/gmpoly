The ‘gmpoly’ package
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/stla/gmpoly/workflows/R-CMD-check/badge.svg)](https://github.com/stla/gmpoly/actions)
<!-- badges: end -->

*R package for multivariate polynomials with rational coefficients.*

``` r
library(gmpoly)
```

Define a polynomial with the `gmpoly` function:

``` r
pol <- gmpoly("4 x^(2, 1, 1) + 1/2 x^(0,1,0)")
pol
## [1] "1/2 x^(0,1,0) + 4 x^(2,1,1)"
```

Some arithmetic on this polynomial:

``` r
-pol
## [1] "-1/2 x^(0,1,0) - 4 x^(2,1,1)"
2 * pol
## [1] "1 x^(0,1,0) + 8 x^(2,1,1)"
pol / 2
## [1] "1/4 x^(0,1,0) + 2 x^(2,1,1)"
pol + 5
## [1] "5 x^(0,0,0) + 1/2 x^(0,1,0) + 4 x^(2,1,1)"
pol - 5
## [1] "-5 x^(0,0,0) + 1/2 x^(0,1,0) + 4 x^(2,1,1)"
pol^2
## [1] "1/4 x^(0,2,0) + 4 x^(2,2,1) + 16 x^(4,2,2)"
```

Two polynomials can be added and multiplied:

``` r
pol1 <- gmpoly("2 x^(1,1) - 5/3 x^(0,1)")
pol2 <- gmpoly("-2 x^(1,1) + 3 x^(2,1)")
pol1 + pol2
## [1] "-5/3 x^(0,1) + 3 x^(2,1)"
pol1 * pol2
## [1] "10/3 x^(1,2) - 9 x^(2,2) + 6 x^(3,2)"
```

Use `gmpolyEval` to evaluate a polynomial for some values of the
variables:

``` r
library(gmp, warn.conflicts = FALSE)
pol <- gmpoly("5/2 x^(2,3) + 3 x^(1,1)")
gmpolyEval(pol, x = as.bigq(c(1, 1)))
## Big Rational ('bigq') :
## [1] 11/2
```

To evaluate the polynomial for several sets of variables, supply a
matrix to the second argument:

``` r
X <- rbind(
  t(as.bigq(c(1, 1))),
  t(as.bigq(c(3, 4), c(4, 3)))
)
gmpolyEval(pol, x = X)
## Big Rational ('bigq') object of length 2:
## [1] 11/2 19/3
```
