\name{as.data.frame-methods}
\docType{methods}
\alias{as.data.frame}
\alias{as.data.frame-methods}
\alias{as.data.frame,hyperSpec,missing,missing-method}
\alias{as.data.frame,hyperSpec-method}
\alias{as.matrix}
\alias{as.matrix-methods}
\alias{as.matrix,ANY-method}
\alias{as.matrix,hyperSpec-method}
\title{Conversion of a hyperSpec object into a data.frame or matrix}
\description{
  \code{as.data.frame} returns \code{x@data}

  \code{as.matrix} returns the spectra matrix \code{x@data$spc}
}
\usage{
\S4method{as.data.frame}{hyperSpec,missing,missing}(x, row.names = NULL, optional = FALSE, \dots)
\S4method{as.matrix}{hyperSpec}(x, \dots)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  \item{row.names, optional, \dots}{ignored}
  %\item{\dots}{ignored}
}
\value{
  \code{x@data} and \code{x@data$spc}, respectively.
}

\author{C. Beleites}
\seealso{
  \code{\link[base]{as.data.frame}} and \code{\link[base]{as.matrix}}

  \code{\link[hyperSpec:extract_replace]{[}}
}
\examples{
as.data.frame (chondro [1:3,, 600:620])
as.matrix (chondro [1:3,, 600:620])
}

\keyword{methods}

