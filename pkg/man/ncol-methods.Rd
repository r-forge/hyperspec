\name{ncol-methods}
\docType{methods}
\alias{ncol}
\alias{ncol,hyperSpec-method}
\alias{nrow}
\alias{nrow,hyperSpec-method}
\alias{nwl}

\title{The Number of Rows (Spectra), Columns, and Data Points per
  Spectrum of an hyperSpec Object)}
\description{
  \code{nrow} yields the number of rows in \code{x@data}, i.e. the
  number of spectra in the \code{hyperSpec} object.
  
 \code{ncol} returns the number of columns in \code{x@data}. I.e. the
  number of columns with additional information to each spectrum
  (e.g. "x", "y", ...) + 1 (for column \code{spc} containing the spectra).

  \code{nwl} returns the number of columns in \code{x@data$spc},
  i.e. the length of each spectrum.
}
\usage{
ncol (x)
nrow (x)
nwl (x)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
}
\value{
  an \code{integer} of length 1 or \code{NULL}.
}

\references{
}
\author{C. Beleites}
\seealso{\code{\link[base]{nrow}} and \code{\link[base]{ncol}}}
\examples{
nrow (chondro)
ncol (chondro)
nwl  (chondro)
}

\keyword{methods}

