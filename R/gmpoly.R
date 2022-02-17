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
  pol
}
