lexorder <- function(M){
  do.call(order, lapply(seq_len(ncol(M)), function(i) M[, i]))
}

lexsort <- function(M){
  M[lexorder(M), ]
}