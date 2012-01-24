\name{colSums}
\alias{colMeans}
\alias{colSums}
\alias{rowMeans}
\alias{rowSums}
\title{functions for hyperSpec objects}
\arguments{
  \item{x}{hyperSpec object}

  \item{label.spc}{labels for the intensity axis for
  loadings-like (col) statistics}

  \item{label.wavelength}{labels for the wavelength axis
  for scores-like (row) statistics}

  \item{user,short,date}{handed to
  \code{\link[hyperSpec]{logentry}}}

  \item{na.rm,...}{further parameters to the base
  functions}
}
\description{
  hyperSpec objects can use the base functions
  \code{\link[base]{colMeans}},
  \code{\link[base]{colSums}}, \code{\link[base]{rowMeans}}
  and \code{\link[base]{rowSums}}.
}
\seealso{
  \link[base]{colSums}
}

