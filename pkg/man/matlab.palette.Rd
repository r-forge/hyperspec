\name{matlab.palette}
\alias{matlab.palette}
\alias{matlab.dark.palette}
\concept{matlab.dark.palette}
\title{Matlab-like Palettes}
\description{
  Two palettes going from blue over green to red, approximately as the
  standard palette of Matlab does. The second one has darker green
  values and is better suited for plotting lines on white background.
}
\usage{
matlab.palette(n = 100, \dots)
matlab.dark.palette(n = 100, \dots)
}
\arguments{
  \item{n}{the number of colors to be in the palette.}
  \item{\dots}{further arguments are handed to \code{\link[grDevices]{rainbow}}}
}
\value{
  A vector containing the color values in the form "\#rrbbggaa".
}
\author{C. Beleites}
\seealso{\code{\link[grDevices]{rainbow}}}
\examples{
plotmap (chondro, trellis.args = list (col.regions = matlab.palette ()))
plot (flu, col = matlab.dark.palette (nrow (flu)))
}
\keyword{color}
