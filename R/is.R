isPositiveInteger <- function(n){
  is.vector(n) && is.numeric(n) && length(n) == 1L && !is.na(n) && floor(n) == n
}