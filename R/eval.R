#' @title Evaluation of a multivariate polynomial
#' @description Evaluates a \code{gmpoly} multivariate polynomial for given 
#'   values of the variables.
#'
#' @param pol a \code{\link{gmpoly}} object
#' @param x either a \code{\link[gmp]{bigq}} vector, or a 
#'   \code{\link[gmp]{bigq}} matrix; in the later case, the evaluation is 
#'   performed for each row of the matrix
#'
#' @return A \code{\link[gmp]{bigq}} number or vector.
#' @export
#' 
#' @importFrom gmp NA_bigq_ apply
#'
#' @examples library(gmpoly)
#' pol <- gmpoly("5/2 x^(2,3) + 3 x^(1,1)")
#' gmpolyEval(pol, as.bigq(c(1, 1)))
#' x <- rbind(
#'   as.bigq(c(1, 1)),
#'   as.bigq(c(3, 4), c(4, 3))
#' )
#' gmpolyEval(pol, x)
gmpolyEval <- function(pol, x){
  stopifnot(inherits(pol, "gmpoly"))
  stopifnot(is.bigq(x))
  nvariables <- pol[["m"]]
  if(!is.matrix(x)){
    stopifnot(length(x) == nvariables)
    x <- rbind(x)
  }else{
    if(ncol(x) != nvariables){
      stop(
        sprintf(
          "Invalid `x` matrix: there are %d variables, but `x` has %d columns.", 
          nvariables, ncol(x))
      )
    }
  }
  powers <- attr(pol, "powers")
  if(is.null(powers)){
    powers <- vapply(pol[["exponents"]], function(e){
      grlexUnrank(nvariables, e)
    }, integer(nvariables))
  }else{
    powers <- t(powers)
  }
  nresults <- nrow(x)
  results <- rep(NA_bigq_, nresults)
  coeffs <- pol[["coeffs"]]
  for(i in 1L:nresults){
    results[i] <- sum(coeffs * gmp::apply(x[i,]^powers, 0L, prod))
  }
  results
}