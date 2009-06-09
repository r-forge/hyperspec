\name{matlab.palette}
\alias{matlab.palette}
\title{Matlab-like Palette}
\description{
  A palette going from blue over green to red, approximately as the
  standard palette of Matlab does.
}
\usage{
matlab.palette(n = 100)
}
\arguments{
  \item{n}{the number of colors to be in the palette.}
}
\value{
  A vector containing the color values in the form "\#rrbbggaa".
}
\author{C. Beleites}
\examples{
plotmap (chondro, trellis.args = list (col.regions = matlab.palette ()))
}
\keyword{color}