\name{sample-methods}
\docType{methods}
\alias{sample-methods}
\alias{sample,ANY-method}
\alias{sample,hyperSpec-method}
\title{Random Samples and Permutations}
\description{
Take a sample of the specified size from the elements of x with or without replacement. 
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{The base method
  \code{\link[base]{sample}} is made generic by package hyperSpec.}

\item{\code{signature(x = "hyperSpec")}}{sample spectra of hyperSpec objects}
}}
\usage{
sample(x, size, replace = FALSE, prob = NULL, ..., index = FALSE)
}
\arguments{
  \item{x}{The hyperSpec object to sample from}
  \item{size}{positive integer giving the number of spectra to choose.}
  \item{replace}{Should sampling be with replacement?}
  \item{prob}{A vector of probability weights for obtaining the elements
    of the vector being sampled.}
  \item{index}{Should the return value be a vector of indices or the
	 corresponding hyperSpec object?}
}
\seealso{\code{\link[base]{sample}}}
\value{an index vector if \code{index == TRUE}, or a hyperSpec object.}
\examples{
sample (flu, 3, index = TRUE)
sample (flu, 3, replace = TRUE, index = TRUE)
sample (flu, 8, replace = TRUE, index = TRUE)

plot (flu, col = "darkgray")
plot (sample (flu, 3), col = "red", add = TRUE)

plot (flu, col = "darkgray")
plot (sample (flu, 3, replace = TRUE), col = "#0000FF80", add = TRUE,
      lines.args = list (lwd = 2))
}
\keyword{methods}
\keyword{distribution}

