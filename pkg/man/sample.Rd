\name{sample-methods}
\docType{methods}
\alias{sample-methods}
\alias{sample}
\alias{isample}
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
 \S4method{sample}{hyperSpec}(x, size, replace = FALSE, prob = NULL,
                              short = "sample", user = NULL, date = NULL)
isample (x, size, replace = FALSE, prob = NULL)
}
\arguments{
  \item{x}{The hyperSpec object to sample from}
  \item{size}{positive integer giving the number of spectra to choose.}
  \item{replace}{Should sampling be with replacement?}
  \item{prob}{A vector of probability weights for obtaining the elements
    of the vector being sampled.}
  \item{short, user, date}{are handed to \code{\link{logentry}}}
}
\details{
  \code{isample} returns an vector of indices, \code{sample} returns the
  corresponding hyperSpec object.
}
\author{C. Beleites}
\seealso{\code{\link[base]{sample}}}
\value{a hyperSpec object for \code{sample}, and an integer vector for
  \code{isample} that is suitable for indexing (into the spectra) of x.}
\examples{
sample (flu, 3)
isample (flu, 3)
isample (flu, 3, replace = TRUE)
isample (flu, 8, replace = TRUE)

plot (flu, col = "darkgray")
plot (sample (flu, 3), col = "red", add = TRUE)

plot (flu, col = "darkgray")
plot (sample (flu, 3, replace = TRUE), col = "#0000FF80", add = TRUE,
      lines.args = list (lwd = 2))
}
\keyword{methods}
\keyword{distribution}

