\name{rowSums-methods}
\docType{methods}
\alias{rowSums-methods}
\alias{rowSums,array-method}
\alias{rowSums,matrix-method}
\alias{rowMeans-methods}
\alias{rowMeans,array-method}
\alias{rowMeans,matrix-method}
\alias{colSums-methods}
\alias{colSums,array-method}
\alias{colSums,matrix-method}
\alias{colMeans-methods}
\alias{colMeans,array-method}
\alias{colMeans,matrix-method}
\title{rowSums, rowMeans, etc. with drop}
\description{
  Form row and column sums and means for numeric arrays. 
}
\usage{
\S4method{rowSums}{matrix}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{rowSums}{array}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{rowMeans}{matrix}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{rowMeans}{array}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{colSums}{matrix}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{colSums}{array}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{colMeans}{matrix}(x, na.rm = FALSE, dims = 1, drop = TRUE)
\S4method{colMeans}{array}(x, na.rm = FALSE, dims = 1, drop = TRUE)
}
\arguments{
  \item{x}{an array of two or more dimensions, containing numeric,
    complex, integer or logical values, or a numeric data frame.}
  \item{na.rm}{logical. Should missing values (including \code{NaN})
    be omitted from the calculations?}
  \item{dims}{integer: Which dimensions are regarded as \sQuote{rows} or
    \sQuote{columns} to sum over.  For \code{row*}, the sum or mean is
    over dimensions \code{dims+1, \dots}; for \code{col*} it is over
    dimensions \code{1:dims}.}
  \item{drop}{If \code{FALSE}, the number of dimensions is retained: the
  length of the dimensions that are summed or averaged is set to
  1. \code{TRUE} yield the same behaviour as \code{\link[base]{colSums}}}
}
\seealso{\code{\link[base]{colSums}}}
\examples{
a <- array (1 : 24, 4 : 2)
a

rowSums (a)
rowSums (a, drop = FALSE)

colSums (a)
colSums (a, drop = FALSE)

colSums (a, dim = 2)
colSums (a, dim = 2, drop = FALSE)
}
\keyword{array}
\keyword{algebra}
\keyword{arith}
