\name{colSums}
\alias{colMeans,hyperSpec-method}
\alias{colSums}
\alias{colSums,hyperSpec-method}
\alias{rowMeans,hyperSpec-method}
\alias{rowSums,hyperSpec-method}
\title{functions for hyperSpec objects}
\usage{
  \S4method{colMeans}{hyperSpec}(x, na.rm = TRUE, dims = 1,
    ...)

  \S4method{colSums}{hyperSpec}(x, na.rm = TRUE, dims = 1,
    ...)

  \S4method{rowMeans}{hyperSpec}(x, na.rm = TRUE, dims = 1,
    ...)

  \S4method{rowSums}{hyperSpec}(x, na.rm = TRUE, dims = 1,
    ...)
}
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

  \item{na.rm}{logical.  Should missing values (including
  \code{NaN}) be omitted from the calculations?}

  \item{dims}{integer: Which dimensions are regarded as
  \sQuote{rows} or \sQuote{columns} to sum over.  For
  \code{row*}, the sum or mean is over dimensions
  \code{dims+1, \dots}; for \code{col*} it is over
  dimensions \code{1:dims}.}
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

