\name{wl2relwn}
\alias{wl2relwn}
\title{Wavelength to Raman shift conversion.}
\usage{
  wl2relwn(spc, laser)
}
\arguments{
  \item{spc}{hyperSpec object with wavelength axis holding
  wavelength in nm.}

  \item{laser}{wavelength of the excitation laser in nm}
}
\value{
  hyperSpec object with wavelength axis holding Raman shift
  (relative wavenumbers) in 1/cm
}
\description{
  Convert a hyperSpec object's wavelegth axis from
  wavelength in nm to Raman relative wavenumbers in 1/cm.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{wlcal.Kaiser}}
}

