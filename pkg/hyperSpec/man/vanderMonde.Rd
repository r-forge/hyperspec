\name{vanderMonde}
\alias{normalize01}
\alias{vanderMonde}
\alias{vanderMonde,hyperSpec-method}
\alias{wl.eval}
\title{Function evaluation on hyperSpec objects}
\usage{
  vanderMonde(x, order, ...)

  \S4method{vanderMonde}{hyperSpec}(x, order, ...,
    normalize.wl = I)

  wl.eval(x, ..., normalize.wl = I)

  normalize01(x)
}
\arguments{
  \item{x}{vector with values to evaluate the polynomial on

  vector with values to transform

  NA}

  \item{order}{of the polynomial}

  \item{\dots}{ignored by the standard generic

  hyperSpec method: further arguments to
  \code{\link{decomposition}}

  hyperSpec method: further arguments to
  \code{\link{decomposition}}}

  \item{normalize.wl}{function to transorm the wavelengths
  before evaluating the polynomial (or other function). Use
  \code{normalize01} to map the wavelength range to the
  interval [0, 1].}

  \item{x}{hyperSpec object}
}
\value{
  van der Monde matrix

  hyperSpec method: hyperSpec object containing van der
  Monde matrix as spectra and an additional column
  ".vdm.order" giving the order of each spectrum (term).

  hyperSpec object containing one spectrum for each
  expression

  vector with \code{x} values mapped to the interval [0,
  1]: (x - min (x)) / diff (range (x))
}
\description{
  vandermonde generates van der Monde matrices for the
  wavelengths of a hyperSpec object.
}
\examples{
plot (vanderMonde (flu, 2))
plot (vanderMonde (flu, 2, normalize.wl = normalize01))
plot (wl.eval (laser, exp = function (x) exp (-x)))
}
\author{
  C. Beleites
}

