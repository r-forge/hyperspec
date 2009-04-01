\name{spc.fit.poly}
\alias{spc.fit.poly}
\alias{spc.fit.poly.below}
\title{Polynomial Baseline Fitting}
\description{
  These functions fit polynomal baselines. 
}
\usage{
spc.fit.poly(fit, apply = NULL, poly.order = 1, 
             short = NULL, user = NULL, date = NULL)
spc.fit.poly.below  (fit, apply = fit, poly.order = 1, npts.min = NULL,
                     noise = numeric (nrow (fit)), 
                     short = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{fit}{\code{hyperSpec} object on which the baselines are fitted}
  \item{apply}{\code{hyperSpec} object on which the baselines are evaluted
  If \code{NULL}, a \code{hyperSpec} object containing the polynom coefficients 
  rather than evaluted baselines is returned.} 
  \item{poly.order}{order of the polynom to be used}
  \item{npts.min}{minmal number of points used for fitting the polynom}
  \item{noise}{noise level to be considered for each of the spectra}
  \item{short, user, date}{handed to \code{logentry}}
}
\details{
  Both functions fit polynoma to be used as baselines. If apply is \code{NULL}, 
  a \code{hyperSpec} object with the polynom coefficients is returned, 
  otherwise the polynoma are evaluated on the spectral range of \code{apply}.
  
  \code{spc.fit.poly} calculates the least squares fit of grade \code{poly.order} 
  through the complete spectra given in \code{fit}. Thus \code{fit} needs to be cut appropriately.
  
  \code{spc.fit.poly.below} tries to fit the baseline on complete spectra. For details, see the \link{baselinebelow}.
}
\value{
  \code{hyperspec} object containing the baselines in the spectra matrix,
  either as polynom coefficients or as polynoms evaluted on the spectral range of \code{apply} 
}
\author{C. Beleites}
\examples{
}
\keyword{manip}
\keyword{datagen}
