\name{which.quantile}
\alias{which.quantile}
\title{indices of quantiles}
\usage{
  which.quantile(x, probs, na.rm = FALSE)
}
\arguments{
  \item{x}{data}

  \item{probs}{which quantiles to return}

  \item{na.rm}{should \code{NA}s be removed before?}
}
\value{
  vector of indices corresponding to the quantiles
}
\description{
  Similar to \code{\link[stats]{which.min}} and
  \code{\link[stats]{which.max}}. The indices correspond to
  \code{\link[stats]{quantile}} type 3.
}
\examples{
x <-c (2.34, 5.83, NA, 9.34, 8.53, 6.42, NA, 8.07, NA, 0.77)
probs <- c (0, .23, .5, .6, 1)

which.quantile (x, probs, na.rm = TRUE)

x [which.quantile (x, probs, na.rm = TRUE)] == quantile (x, probs, na.rm = TRUE, type = 3)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[stats]{quantile}}
}

