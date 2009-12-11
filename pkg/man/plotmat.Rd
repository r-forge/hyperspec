\name{plotmat}
\alias{plotmat}
\title{Plotting the spectra matrix}
\description{
\code{plotmat} produces a   \code{\link[lattice]{levelplot}} with the 
wavelength axis as abscissa and one extra data column as ordinate. The 
intensity is colour-coded.
}
\usage{
plotmat(object, use.t = "t", cond = NULL, ...)
}
\arguments{
  \item{object}{a hyperSpec object
}
  \item{use.t}{name of the extra data column to use as ordinate}
  \item{cond}{character giving conditioning variable(s)}
  \item{\dots}{further arguments to \code{\link[lattice]{levelplot}}}
}
\value{
the lattice object produced by \code{\link[lattice]{levelplot}}
}
\author{
C. Beleites
}
\seealso{\code{\link[lattice]{levelplot}}}
\examples{
plotmat (laser, col.regions = matlab.palette (20))	
plotmat (flu, use.t = "c", contour = TRUE, col = "#00000080", labels = TRUE)	
}
\keyword{hplot}
