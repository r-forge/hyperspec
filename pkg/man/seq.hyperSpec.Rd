\name{seq.hyperSpec}
\alias{seq.hyperSpec}
\title{Sequence generation along spectra or wavelengths}
\description{
This function generates sequences along the spectra (rows) or
wavelengths of hyperSpec objects.  
}
\usage{
seq.hyperSpec(x, ..., along.with = "spc")
}
\arguments{
  \item{x}{ the hyperSpec object}
  \item{\dots}{arguments handed to \code{\link[base]{seq.int}},
	 particularly, \code{from}, \code{to}, \code{by}, and \code{length.out}.}
  \item{along.with}{
	 character indicating the \emph{direction} of the sequence:

	 \code{"spc"} sequence over the spectra

	 \code{"wl"} sequence over the wavelengths

	 \code{"wli"} sequence over the wavelength indices
}
}
\details{
  Note that \code{\link{wl2i}} also generates sequences of indices along
  the wavelength axis. 
}
\value{
  a \code{numeric}
}
\author{
C. Beleites
}
\seealso{
 \code{\link{wl2i}} for further possibilities to construct sequences of
 wavelength indices.

 \code{\link[base]{seq.int}}
}
\examples{
seq (flu)
seq (flu, length.out = 3) 		  # return value is numeric, not integer!
seq (flu, by = 2) 		        # return value is numeric, not integer! 

seq (flu, length.out = 5, along.with = "wli")
seq (flu, length.out = 5, along.with = "wl")

# from and to are interpreted as indices. 
seq (flu, to = 10, by = 0.5, along.with = "wli")
}
\keyword{manip}
