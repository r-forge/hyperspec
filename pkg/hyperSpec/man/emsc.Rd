\name{emsc}
\alias{emsc}
\alias{emsc,hyperSpec,hyperSpec-method}
\alias{spc.evalfun}
\title{Function for doing EMSC on hyperSpec objects}
\usage{
  emsc(X, Reference = NULL, bg.comps = NULL,
    norm.comps = NULL, ...)

  \S4method{emsc}{hyperSpec,hyperSpec}(X, Reference,
    bg.comps, norm.comps, ...)

  spc.evalfun(X, f, ...)
}
\arguments{
  \item{X}{spectra matrix in rows}

  \item{Reference}{matrix with reference spectra rows}

  \item{bg.comps}{indices of background components}

  \item{norm.comps}{indices of components to use for
  normalization}

  \item{\dots}{hyperSpec method: further arguments to
  \code{\link{decomposition}}

  hyperSpec method: further arguments to f}
}
\value{
  new spectra matrix

  hyperSpec method: hyperSpec object containing emsc
  corrected spectrum of input hyperSpec objects: X and
  Reference, vectors: bg.comps and norm.comps, indices.

  hyperSpec method: hyperSpec object containing emsc
  corrected spectrum of input hyperSpec objects: X and
  Reference, vectors: bg.comps and norm.comps, indices.
}
\description{
  Function for doing EMSC on hyperSpec objects
}
\examples{
vmflu <- vanderMonde(flu,2)
Refs <- rbind(1, jitter(flu[[]][1,], 100), vanderMonde(flu,2,normalize.wl = normalize01)[[]][c(2,3),])
nuflu <- emsc(flu, Refs, 1, c(2,3))
}
\author{
  S. Fuller
}
\references{
  Martens, H.; Nielsen, J. P. and Engelsen, S. B.: Light
  Scattering and Light Absorbance Separated by Extended
  Multiplicative Signal Correction. Application to
  Near-Infrared Transmission Analysis of Powder Mixtures,
  Analytical Chemistry (2003) 75, 394-404.
}
\seealso{
  \code{\link[pls]{msc}} was used as starting point for
  this function.
}

