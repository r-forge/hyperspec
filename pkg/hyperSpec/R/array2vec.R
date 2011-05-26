###-----------------------------------------------------------------------------
###
###  array2vec -- array index to vector index conversion
###
###

array2vec <- function (iarr, dim){
  if (!is.matrix (iarr))
    dim (iarr) <- c(1, length (iarr))

  if (ncol (iarr) != length (dim))
    stop ("Number of columns in iarr and number of dimensions differ.")

  if (any (sweep (iarr, 2, dim) > 0))
    stop ("array index > dim")

  pdim <- c(1, cumprod (dim [- length (dim)]))
  iarr <- iarr - 1

  colSums(apply (iarr, 1, "*", pdim)) + 1
}
