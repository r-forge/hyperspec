\name{spc.fit.poly}
\alias{spc.fit.poly}
\alias{spc.fit.poly.below}
\title{Polynomial Baseline Fitting}
\description{
  These functions fit polynomal baselines. 
}
\usage{
spc.fit.poly(fit, apply, poly.order = 1)
spc.fit.poly.below  (fit, apply = fit, poly.order = 1,
                     npts.min = NULL,
                     noise = numeric (nrow (fit)))
}
\arguments{
  \item{fit}{ ~~Describe \code{fit} here~~ }
  \item{apply}{ ~~Describe \code{fit} here~~ }
  \item{poly.order}{ ~~Describe \code{\dots} here~~ }
  \item{npts.min}{}
  \item{noise}{}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  \item{bl}{\code{hyperspec} object containing the baselines in the
	 spectra matrix.}
  \item{p}{\code{hyperSpec} object containing the polynom coefficients
	 in the spectra matrix.}
}
\references{ ~put references to the literature/web site here ~ }
\author{C. Beleites}
\examples{
}
\keyword{manip}
\keyword{datagen}
