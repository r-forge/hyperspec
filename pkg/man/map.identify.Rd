\name{map.identify}
\alias{map.identify}
\title{Identify Spectra on a Map Plot}
\description{
  Identify spectra on a map plot by clicking at the points.  Click with
  button other than left to end.
}
\usage{
map.identify(x, lattice = NULL)
}
\arguments{
  \item{x}{a hyperSpec object}
  
  \item{lattice}{a lattice object as returned by
	 \code{\link{plotmap}}. If missing, \code{\link{plotmap} (x)} is
	 used.} 
}
\value{
  A vector with the row indices into \code{x} of the identified points.
}
\author{C. Beleites}
\seealso{\code{\link{plotmap}}, \code{\link[lattice]{panel.identify}}}
\examples{
map.identify (chondro)
}
\keyword{iplot}

