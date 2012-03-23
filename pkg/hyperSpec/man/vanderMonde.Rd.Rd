\name{vanderMonde}
\alias{vanderMonde}
\alias{vanderMonde,hyperSpec-method}
\alias{wl.eval}
\title{Function evaluation on hyperSpec objects}
\usage{
  vanderMonde(x, order, ...)

  \S4method{vanderMonde}{hyperSpec}(x, order, ...)

  wl.eval(x, ...)
}
\arguments{
  \item{x}{

  NA}

  \item{order}{of the polynomial}

  \item{\dots}{ignored by the standard generic

  hyperSpec method: further arguments to
  \code{\link{decomposition}}

  hyperSpec method: further arguments to
  \code{\link{decomposition}}}

  \item{x}{hyperSpec object}
}
\value{
  van der Monde matrix

  hyperSpec method: hyperSpec object containing van der
  Monde matrix as spectra and an additional column
  ".vdm.order" giving the order of each spectrum (term).

  hyperSpec object containing one spectrum for each
  expression
}
\description{
  vandermonde generates van der Monde matrices for the
  wavelengths of a hyperSpec object.
}
\examples{
plot (wl.eval (laser, exp = function (x) exp (-x)))
}
\author{
  C. Beleites
}

