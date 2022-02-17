#' @title Define a multivariate polynomial
#' @description Define a multivariate polynomial from a string or from its 
#'   coefficients and powers. 
#'
#' @param string a string such as \code{"x^(1,2,1) + 4 x^(0,2,3)"}, or you can 
#'   define the polynomial with \code{coeffs} and \code{powers}; in this case
#'   set \code{string} to \code{NULL} or to nothing (i.e. missing)
#' @param coeffs the coefficients of the polynomial, in case you don't define 
#'   it with the \code{string} argument; this must be a vector of 
#'   \code{\link[gmp]{bigq}} numbers
#' @param powers the powers of the polynomial, in case you don't define it 
#'   with the \code{string} argument; this must be a matrix of integers, one 
#'   row for each term (hence \code{nrow(powers)} must equal 
#'   \code{length(coeffs)}), and the number of columns is the number of 
#'   variables
#'
#' @return A \code{gmpoly} object.
#' @export
#' 
#' @importFrom gmp is.bigq
#'
#' @examples library(gmpoly)
#' gmpoly("5/2 x^(2,3) + 3 x^(1,1)")
#' gmpoly("5/2 x^(2,3) - 3 x^(1,1)")
#' gmpoly("-x^(1,2,3) + 4/7 x^(3,1,1)")
#' library(gmp)
#' gmpoly(
#'   coeffs = as.bigq(c(5, 7), c(8, 9)), 
#'   powers = rbind(c(1, 1, 1), c(2, 2, 2))
#' )
gmpoly <- function(string, coeffs = NULL, powers = NULL){
  if(missing(string) || is.null(string)){
    stopifnot(is.bigq(coeffs))
    stopifnot(is.matrix(powers))
    stopifnot(length(coeffs) == nrow(powers))
    storage.mode(powers) <- "integer"
    exponents <- apply(powers, 1L, grlexRank)
    m <- ncol(powers)
    pol <- list(
      "coeffs" = coeffs,
      "exponents" = exponents,
      "m" = m
    )
    attr(pol, "string") <- polAsString(pol, drop(t(powers)))
  }else{
    pol <- stringToPol(string)
  }
  class(pol) <- "gmpoly"
  pol
}

#' @title Print a multivariate polynomial
#' @description Print a multivariate polynomial of class \code{gmpoly}
#'
#' @param x a \code{\link{gmpoly}} object
#' @param ... ignored
#'
#' @return No value, just prints the polynomial.
#' @export
print.gmpoly <- function(x, ...){
  if(is.null(attr(x, "string"))){
    print(polAsString(x))
  }else{
    print(attr(x, "string"))
  }
}
