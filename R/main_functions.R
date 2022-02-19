polynomialSort <- function(pol){
  powers <- pol[["powers"]]
  idx <- lexorder(powers)
  list(
    "coeffs" = pol[["coeffs"]][idx],
    "powers" = powers[idx, , drop = FALSE],
    "m" = pol[["m"]]
  )
}


#' @importFrom gmp NA_bigq_
#' @noRd
polynomialCompress <- function(pol){
  coeffs <- pol[["coeffs"]]
  # zeros <- coeffs == 0L
  # if(any(zeros)){
  #   coeffs <- coeffs[!zeros]
  #   exponents <- pol[["exponents"]][!zeros]
  # }else{
  powers <- pol[["powers"]]
  # }
  o1 <- o2 <- nrow(powers)
  powers2 <- matrix(NA_integer_, nrow = o2, ncol = pol[["m"]])
  coeffs2 <- rep(NA_bigq_, o2)
  get <- put <- 0L
  while(get < o1){
    get <- get + 1L
    if(0L == put){
      put <- put + 1L
      coeffs2[put] <- coeffs[get]
      powers2[put, ] <- powers[get, ]
    }else{
      if(all(powers2[put, ] == powers[get, ])){
        coeffs2[put] <- coeffs2[put] + coeffs[get]
      }else{
        put <- put + 1L
        coeffs2[put] <- coeffs[get]
        powers2[put, ] <- powers[get, ]
      }
    }
  }
  o2 <- put
  get <- put <- 1L
  while(get <= o2){
    if(coeffs2[get] != 0L){
      coeffs2[put] <- coeffs2[get]
      powers2[put, ] <- powers2[get, ]
      put <- put + 1L
    }
    get <- get + 1L
  }
  o2 <- put - 1L
  list(
    "coeffs" = coeffs2[1L:o2],
    "powers" = powers2[1L:o2, , drop = FALSE],
    "m" = pol[["m"]]
  )
}


polynomialAdd <- function(pol1, pol2){
  m <- pol1[["m"]]
  if(m != pol2[["m"]]){
    stop(
      "Adding polynomials is possible only if the number of variables is the ",
      "same for the two polynomials.",
      call. = TRUE
    )
  }
  if(isZeroPol(pol1)){
    return(pol2)
  }
  if(isZeroPol(pol2)){
    return(pol1)
  }
  coeffs <- c(pol1[["coeffs"]], pol2[["coeffs"]])
  powers1 <- attr(pol1, "powers")
  powers2 <- attr(pol2, "powers")
  powers <- rbind(pol1[["powers"]], pol2[["powers"]])
  pol <- polynomialSort(list(
    "coeffs" = coeffs,
    "powers" = powers,
    "m" = m
  ))
  notCompressed <- anyDuplicated(powers) || any(coeffs == 0)
  if(notCompressed){
    pol <- polynomialCompress(pol)
  }
  class(pol) <- "gmpoly"
  pol
}

#' @importFrom gmp outer
#' @noRd
polynomialMul <- function(pol1, pol2){
  m <- pol1[["m"]]
  if(m != pol2[["m"]]){
    stop(
      "Multiplying polynomials is possible only if the number of variables is ",
      "the same for the two polynomials.",
      call. = TRUE
    )
  }
  if(isZeroPol(pol1) || isZeroPol(pol2)){
    return(zeroPol(m))
  }
  coeffs1 <- pol1[["coeffs"]]
  coeffs2 <- pol2[["coeffs"]]
  o1 <- length(coeffs1)
  o2 <- length(coeffs2)
  powers1 <- pol1[["powers"]]
  powers2 <- pol2[["powers"]]
  coeffs <- c(gmp::outer(coeffs1, coeffs2))
  nterms <- o1 * o2
  powers <- matrix(NA_integer_, nrow = nterms, ncol = m)
  o <- 1L
  for(j in 1L:o2){
    for(i in 1L:o1){
      powers[o, ] <- powers1[i, ] + powers2[j, ]
      o <- o + 1L
    }
  }
  pol <- polynomialSort(list(
    "coeffs" = coeffs,
    "powers" = powers,
    "m" = m
  ))
  notCompressed <- anyDuplicated(powers) || any(coeffs == 0)
  if(notCompressed){
    pol <- polynomialCompress(pol)
  }
  class(pol) <- "gmpoly"
  pol
}
