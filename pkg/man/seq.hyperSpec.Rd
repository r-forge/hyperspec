\name{seq.hyperSpec}
\alias{seq.hyperSpec}
\title{Sequence generation along spectra or wavelengths}
\description{
This function generates sequences along the spectra (rows) or
wavelengths of hyperSpec objects.  
}
\usage{
seq.hyperSpec(x,  from = 1, to = nrow (x), \dots, index = FALSE,
              short = "seq", user = NULL, date = NULL)
}
\arguments{
  \item{x}{ the hyperSpec object}

  \item{from, to}{arguments handed to \code{\link[base]{seq.int}}}

  \item{\dots}{arguments for \code{\link[base]{seq}}, namely \code{by},
	 \code{length.out}}

  \item{index}{should a vector with indices be returned rather than a
	 hyperSpec object?}

  \item{short, user, date}{handed to \code{\link{logentry}}.}
}
\details{
  Note that \code{\link{wl2i}} generates sequences of indices along the wavelength axis. 
}
\value{
  a numeric or hyperSpec object, depending on \code{index}.
}
\author{C. Beleites}
\seealso{
 \code{\link{wl2i}} to construct sequences of wavelength indices.

 \code{\link[base]{seq.int}}
}
\examples{
seq (flu, index = TRUE)
seq (flu, length.out = 3, index = TRUE) # return value is numeric, not integer! 
seq (flu, by = 2, index = TRUE) 		    # return value is numeric, not integer! 

plot (flu, col = "darkgray") 
plot (seq (flu, by = 2), add = TRUE, col= "red")
plot (seq (flu, length.out = 2), add = TRUE, col= "blue")
}
\keyword{manip}
