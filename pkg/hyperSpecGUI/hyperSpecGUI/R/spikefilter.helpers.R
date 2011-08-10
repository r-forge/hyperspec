##
## spikefilter.gui helper functions
##


##' Calculate spike suspiciousness
##'
##' These functions calculate the suspiciousness of data points by a
##' \code{c (-1, 2, -1)} filter. \code{spikefilter} applies this filter along
##' the spectrtal (wavelength) direction, \code{spikefilter2d} in addition also
##' to neighbour spectra.
##' 
##' The recognition of spikes may be greatly improved by preprocesing the
##' spectra specially for this task, see the demo.
##' @rdname spikefilter
##' @param spcmatrix spectra matrix
##' @return matrix of the same size as \code{spcmatrix}
##' @export
spikefilter2d <- function (spcmatrix) {
  ## expand matrix by one row and col at each side
  spcmatrix <- spcmatrix [  c (1, seq_len (nrow (spcmatrix)), nrow (spcmatrix)), ]
  spcmatrix <- spcmatrix [, c (1, seq_len (ncol (spcmatrix)), ncol (spcmatrix))  ]

  # filter 
  d <- t  (apply (spcmatrix, 1, filter, c(-1, 2, -1)))
  d <- d + apply (spcmatrix, 2, filter, c(-1, 2, -1)) 

  # the extra row and col are now NA, so don't return them
  d [-c (1, nrow (d)), -c (1, ncol (d))]
}

##' @rdname spikefilter
##' @export
spikefilter <- function (spcmatrix) {
  ## expand matrix 
  spcmatrix <- spcmatrix [c(1, seq_len (nrow (spcmatrix)), nrow (spcmatrix)), ]

  d <- t (apply (spcmatrix, 1, filter, c(-1, 2, -1)))

  d [, -c (1, ncol (d))]
}

