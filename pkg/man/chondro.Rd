\name{chondro}
\alias{chondro}
\docType{data}
\title{Raman spectra of 2 Chondrocytes in Cartilage}
\description{
A Raman-map (laterally resolved Raman spectra). The data set is used in
the ******Vignette*********  
}
\usage{data(chondro)}
\format{
The data set has 796 Raman spectra measured on a 23 \eqn{\times}{x} 35 grid with 1
micron step size. Spatial information is in \code{chondro$x} and \code{chondro$y}.
Each spectrum has 424 data points in the range of 600 - 1800 \eqn{cm^{-1}}{cm^-1}. 
}
\details{
  see \link{Vignette}
}
\author{A. Bonifacio and C. Beleites}
\examples{
data(chondro)

chondro
}
\keyword{datasets}
