\name{cartilage}
\alias{cartilage}
\alias{cartilage}
\title{Spiky cartilage spectra...}
\description{Spiky cartilage spectra}
\details{A selection of Raman spectra, the first half with spikes, the second half
contains also spectra without spikes.}
\docType{data}
\author{A. Bonifacio and C. Beleites}
\keyword{datasets}
\examples{
cartilage
plot (sample (cartilage, 10))
plotvoronoi (cartilage, func = max, col.regions = alois.palette)
}

