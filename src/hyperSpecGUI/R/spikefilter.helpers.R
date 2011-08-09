##
## spikefilter.gui helper functions
##


spikefilter2d <- function (spcmatrix) {
  ## expand matrix by one row and col at each side
  spcmatrix <- spcmatrix [c (1, seq_len (nrow (spcmatrix)), nrow (spcmatrix)), ]
  spcmatrix <- spcmatrix [, c (1, seq_len (ncol (spcmatrix)), ncol (spcmatrix))]

  # filter 
  d <- t  (apply (spcmatrix, 1, filter, c(-1, 2, -1)))
  d <- d + apply (spcmatrix, 2, filter, c(-1, 2, -1)) 

  # the extra row and col are now NA, so don't return them
  d [-c (1, nrow (d)), -c (1, ncol (d))]
}


spikefilter <- function (spcmatrix) {
  ## expand matrix 
  spcmatrix <- spcmatrix [c(1, seq_len (nrow (spcmatrix)), nrow (spcmatrix)), ]

  d <- t (apply (spcmatrix, 1, filter, c(-1, 2, -1)))

  d [, -c (1, ncol (d))]
}

