\name{normalize01}
\alias{normalize01}
\title{Normalize numbers -> [0, 1]}
\usage{
  normalize01(x)
}
\arguments{
  \item{x}{vector with values to transform}
}
\value{
  vector with \code{x} values mapped to the interval [0,
  1]: (x - min (x)) / diff (range (x))
}
\description{
  The input \code{x} is mapped to [0, 1] by subtracting the
  minimum and subsequently dividing by the maximum.
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[hyperSpec]{wl.eval}},
  \code{\link[hyperSpec]{vanderMonde}}
}

