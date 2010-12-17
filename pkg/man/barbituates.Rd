\name{barbituates}
\alias{barbituates}
\docType{data}
\title{Barbituates Spectra from .spc example files}
\description{
A time series of mass spectra in a list of hyperSpec objects.
}
\usage{data(barbituates)}
\format{
  The data sets consists of 286 spectra. They the BARBITUATES.SPC
  example data from Thermo Galactic's 
}
\author{C. Beleites}
\examples{
barbituates [1:3]
length (barbituates)

barb <- collapse (barbituates)
barb <- orderwl (barb)

plot (barb [1:3], lines.args = list (type = "h"),
      col = matlab.dark.palette (3), stacked = TRUE,
      stacked.args = list (add.factor = .2))

if (require (latticeExtra)){
plot (log (barb), "mat", panel = panel.levelplot.points, cex = 0.3,
      col = "#00000000", col.regions = matlab.palette (20))
}

plotc (apply (barb [,, 42.9~43.2], 1, sum, na.rm = TRUE), spc ~ z,
       panel = panel.lines, ylab = expression (I[m/z == 43] / "a.u."))
}
\keyword{datasets}
