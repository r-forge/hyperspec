\name{hyperSpec-package}
\alias{hyperSpec-package}
\alias{hyperSpec}
\docType{package}
\title{hyperSpec: Handling hyperSpectral Data}
\description{
This package gives an interface to handle hyperspectral data sets in
R. I.e. spatially or time-resolved spectra, or spectra with any other
kind of information associated with the spectra.

The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES,
NIR, IR, Raman, NMR, MS, etc. More generally, any data that is recorded
over a discretized variable, e.g. absorbance = f (wavelength), stored as
a vector of absorbance values for discrete wavelengths is suitable.  
}
\details{
\tabular{ll}{
Package: \tab hyperSpec\cr
Type: \tab Package\cr
Version: \tab 0.5beta\cr
Date: \tab 2009-02-04\cr
License: \tab LGPL\cr
LazyLoad: \tab yes\cr
Depends: \tab methods, lattice\cr
}

}
\author{
  C. Beleites \url{cbeleite@units.it}
}
\references{
~~ Literature or other references for background information ~~
}

\examples{
# information about a hyperSpec object
chondro

# plotting spectra: mean $\pm$ 1 standard deviation
plot (chondro, "spcmeansd")

# plotting a map: integral intensity
plot (chondro, "map")

}

\keyword{ package }
