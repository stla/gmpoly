Ops.gmpoly <- function(e1, e2 = NULL) {
  oddfunc <- function(...){stop("odd---neither argument has class mvp?")}
  unary <- nargs() == 1L
  lclass <- nchar(.Method[1L]) > 0L
  rclass <- !unary && (nchar(.Method[2L]) > 0L)
  
  if(unary){
    if(.Generic == "+"){
      return(e1)
    }else if(.Generic == "-") {
      return(gmpoly_negate(e1))
    }else{
      stop("Unary operator '", .Generic, "' is not implemented for gmpolys.")
    }
  }
  
  if(!is.element(.Generic, c("+", "-", "*", "/", "^", "==", "!="))){
    stop("Operator '", .Generic, "' is not implemented for gmpolys.")
  }
  
  if(.Generic == "*"){
    if(lclass && rclass) {
      return(polynomialMul(e1, e2))
    }else if(lclass){
      return(gmpoly_times_scalar(e1, e2))
    }else if(rclass){
      return(gmpoly_times_scalar(e2, e1))
    }else{
      stop()
    }
  }else if(.Generic == "+"){
    if(lclass && rclass){
      return(polynomialAdd(e1, e2))
    }else if(lclass){
      return(gmpoly_plus_scalar(e1, e2))
    }else if(rclass){
      return(gmpoly_plus_scalar(e2, e1))
    } else {
      stop()
    }
  }else if(.Generic == "-"){
    if(lclass && rclass){
      return(polynomialAdd(e1, gmpoly_negate(e2)))
    }else if(lclass){
      return(gmpoly_plus_scalar(e1, -e2))
    }else if(rclass){
      return(gmpoly_plus_scalar(gmpoly_negate(e2), e1))
    } else {
      stop()
    }
  }else if(.Generic == "^"){
    if(lclass && !rclass){
      return(gmpoly_power(e1, e2))
    }else{
      stop("Generic '^' not implemented in this case.")
    }
  } else if (.Generic == "==") {
    if(lclass && rclass){
      return(mvp_eq_mvp(e1,e2))
    } else {
      stop("Generic '==' only compares two mvp objects with one another")
    }
  } else if (.Generic == "!=") {
    if(lclass && rclass){
      return(!mvp_eq_mvp(e1,e2))
    } else {
      stop("Generic '==' only compares two mvp objects with one another")
    }
  } else if (.Generic == "/") {
    if(lclass && !rclass){
      return(mvp_times_scalar(e1,1/e2))
    } else {
      stop("don't use '/', use ooom() instead")
    }
  }
}

gmpoly_negate <- function(pol){
  if(is.zero(pol)){
    pol
  }else{
    pol[["coeffs"]] <- -pol[["coeffs"]]
    pol
  }
}

gmpoly_times_scalar <- function(pol, lambda){
  pol[["coeffs"]] <- lambda * pol[["coeffs"]]
  pol
}

# `mvp_plus_mvp` <- function(S1,S2){
#   if(is.zero(S1)){
#     return(S2)
#   } else if(is.zero(S2)){
#     return(S1)
#   } else {
#     jj <- mvp_add(
#       allnames1=S1[[1]],allpowers1=S1[[2]],coefficients1=S1[[3]],
#       allnames2=S2[[1]],allpowers2=S2[[2]],coefficients2=S2[[3]]
#     )
#     return(mvp(jj[[1]],jj[[2]],jj[[3]]))
#   }
# }

gmpoly_plus_scalar <- function(pol, x){
  pol[["coeffs"]] <- pol[["coeffs"]] + x
  pol
}

gmpoly_power <- function(pol, n){
  stopifnot(n==round(n))
  if(n<0){
    stop("negative powers not implemented")
  }else if(n==0){
    m <- pol[["m"]]
    gmpoly(sprintf("x^(%s)", toString(rep("0", m))))
  }else{
    Reduce(polynomialMul, rep(list(pol), n))
  }
}

`mvp_eq_mvp` <- function(S1,S2){
  is.zero(S1-S2)  # nontrivial; S1 and S2 might have different orders
}