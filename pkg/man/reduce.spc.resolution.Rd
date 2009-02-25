\name{reduce.spc.resolution}
\alias{reduce.spc.resolution}
\title{Wavelength Binning}
\description{
  In order to reduce the spectral resolution and thus gain signal to
  noise ratio or to reduce the dimensionality of the spectral data set,
  the spectral resolution can be reduced.
}
\usage{
reduce.spc.resolution(hyperSpec, by = stop ("reduction factor needed"), ...)
}
\arguments{
  \item{hyperSpec}{the \code{hyperSpec} object}

  \item{by}{reduction factor}
	 
  \item{\dots}{handed to \code{logentry}.}
}
\details{
  The mean of every \code{by} data points in the spectra is calculated,
  \code{NA}s are ignored.
}
\value{
  A \code{hyperSpec} object with \code{ceiling (nwl (hyperSpec) / by)}
  data points per spectrum.
}
\references{ ~put references to the literature/web site here ~ }
\author{C. Beleites}
\examples{
 spc <- reduce.spc.resolution (flu, 5)

 plot (flu[1,,425:475])
 plot (spc[1,,425:475], add = TRUE, col = "blue")

 nwl (flu)
 nwl (spc)
}
