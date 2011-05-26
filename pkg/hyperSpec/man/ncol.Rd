\name{ncol}
\docType{methods}
\alias{ncol,hyperSpec-method}
\alias{nrow,hyperSpec-method}
\alias{nwl}
\alias{dim,hyperSpec-method}
\alias{length,hyperSpec-method}

\title{The Number of Rows (Spectra), Columns, and Data Points per
  Spectrum of an hyperSpec Object)}
\description{
  \code{nrow} yields the number of rows in \code{x@data}, i.e. the
  number of spectra in the \code{hyperSpec} object.
  
 \code{ncol} returns the number of columns in \code{x@data}. I.e. the
  number of columns with additional information to each spectrum
  (e.g. "x", "y", \dots) + 1 (for column \code{spc} containing the spectra).

  \code{nwl} returns the number of columns in \code{x@data$spc},
  i.e. the length of each spectrum.

  \code{dim} returns all three values in a vector.

  \code{length} is a synonym for \code{nrow}. It is supplied so that
  \code{seq_along (x)} returns a sequence to index each spectrum.
}
\usage{
\S4method{ncol}{hyperSpec}(x)
\S4method{nrow}{hyperSpec}(x)
nwl (x)
\S4method{dim}{hyperSpec}(x)
\S4method{length}{hyperSpec}(x)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
}
\value{
  \code{nrow}, \code{ncol}, \code{nwl}, and \code{length},return an \code{integer}.
  
   \code{dim} return a vector of length 3.
}
\author{C. Beleites}
\seealso{\code{\link[base]{nrow}}, \code{\link[base]{ncol}}, 
  \code{\link[base]{dim}}, and   \code{\link[base]{length}}

  \code{\link{seq.hyperSpec}}, and  \code{\link[base]{seq_along}}, 
}

\examples{
nrow (chondro)
ncol (chondro)
nwl  (chondro)
dim (chondro)
length (chondro)
}

\keyword{methods}

