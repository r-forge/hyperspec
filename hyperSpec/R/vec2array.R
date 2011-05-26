###-----------------------------------------------------------------------------
###
###  vec2array -- vector index to array index conversion
###
###  C. Beleites
###

vec2array <- function (ivec, dim) {
  ndim <- length (dim)
  pdim <- c(1, cumprod (dim))

  iarr <- matrix(NA, nrow = length(ivec), ncol = ndim) # matrix for the array indices
  colnames (iarr) <- letters[8 + seq_len (ndim)]       # i, j, k, ...

  ivec <- (ivec - 1)
  for (j in seq_len (ndim))
    iarr [, j] <- (ivec %% pdim [j + 1]) / pdim [j]

  1 + floor(iarr)
}
