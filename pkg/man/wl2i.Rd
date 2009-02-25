\name{wl2i}
\alias{wl2i}
\alias{i2wl}
\title{Conversion between Wavelength and Spectra Matrix Column Index}
\description{
\code{wl2i} returns the column indices for the spectra matrix for the
given wavelengths.

\code{i2wl} converts column indices into wavelengths.
}
\usage{
wl2i(x, wavelength = stop ("wavelengths are required."),
     subst.direct = FALSE)

i2wl(x, i)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  
  \item{wavelength}{the wavelengths to be converted into column indices}

  \item{subst.direct}{if \code{TRUE}, each element of the wavelength
	 vector is converted into the column index.

	 If \code{FALSE}, a sequence from \code{min (wavelength)} to
	 \code{max (wavelength)} is calculated with stepsize as in
	 \code{wavelength}.}
  

  \item{i}{the column indices into the spectra matrix for which the
	 wavelength is to be computed}
}
\value{
  A numeric containing the resulting indices and wavelengths, respectively.
}
\references{ ~put references to the literature/web site here ~ }
\author{C. Beleites}
\examples{
  wl2i (chondro, 600:650)
  wl2i (chondro, seq (600,650,5))
  wl2i (chondro, 600:610, subst.direct = TRUE)

  i2wl (chondro, 17:20)
}
