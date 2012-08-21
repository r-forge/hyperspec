\name{emsc}
\alias{emsc}
\alias{emsc,hyperSpec,hyperSpec-method}
\alias{emsc,hyperSpec,matrix-method}
\alias{emsc,hyperSpec,missing-method}
\alias{emsc,hyperSpec,numeric-method}
\title{Extended Multiplicative Scatter Correction (EMSC)}
\usage{
  emsc(X, Reference = NULL, bg.comps = NULL,
    norm.comps = NULL, ...)

  \S4method{emsc}{hyperSpec,numeric}(X, Reference,
    bg.comps, norm.comps, ...)

  \S4method{emsc}{hyperSpec,matrix}(X, Reference, bg.comps,
    norm.comps, ...)

  \S4method{emsc}{hyperSpec,missing}(X, Reference,
    bg.comps, norm.comps, ...)

  \S4method{emsc}{hyperSpec,hyperSpec}(X, Reference,
    bg.comps, norm.comps, ...)
}
\arguments{
  \item{X}{spectra matrix in rows}

  \item{Reference}{constituents matrix in rows

  matrix with reference spectra rows}

  \item{bg.comps}{background components indices}

  \item{norm.comps}{normalization components indices}

  \item{\dots}{hyperSpec method: further arguments to
  \code{\link{decomposition}}}
}
\value{
  new spectra matrix

  hyperSpec method: hyperSpec object containing emsc
  corrected spectrum of input hyperSpec object X, given
  matrix: Reference, vectors: bg.comps, norm.comps,
  indices.

  hyperSpec method: hyperSpec object containing emsc
  corrected spectrum of input hyperSpec objects: X and
  Reference, vectors: bg.comps and norm.comps, indices.
}
\description{
  This function implements an EMSC similar to the approach
  described in Martens2003 (see below). In contrast to the
  notation there, \bold{all} reference spectra are given in
  one matrix. Arguments \code{bg.comps} and
  \code{norm.comps} specify which spectra belong to
  background components or should be considered for
  normalization.
}
\details{
  Polynomials can be generated with
  \code{\link[hyperSpec]{vanderMonde}}.
}
\examples{
vmflu <- vanderMonde (flu, 2)
Refs <- collapse (normalize01 (flu[6,]), vmflu)
plot (Refs)
plot (emsc (flu, Refs, bg.comps = 2 : 4, norm.comps = 1))
}
\author{
  C.Beleites & S. Fuller
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

  \code{\link[hyperSpec]{vanderMonde}}
}

