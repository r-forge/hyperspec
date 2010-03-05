\name{laser}
\alias{laser}
\docType{data}
\title{Laser Emission}
\description{
A time series of an unstable laser emission.
}
\usage{data(laser)}
\format{
 The data set consists of  84 laser emission spectra measured during 95 min.
 Each spectrum has 36 data points in the range of 404.5 nm to 405.8 nm. 
}
\details{
  see the Vignette
}
\author{C. Beleites}
\examples{
laser

cols <- c ("black", "blue", "#008000", "red")
wl <- i2wl (laser, c(13, 17, 21, 23))
plotspc (laser, axis.args=list (x = list (at = seq (404.5, 405.8, .1))))
for (i in seq_along (wl))
   abline (v = wl[i], col = cols[i], lwd = 2, lty = 2)

plotc (laser [,, wl], spc ~ t, groups = .wavelength, type = "b",
       col = cols)

vignette ("laser", package="hyperSpec")
}
\keyword{datasets}
