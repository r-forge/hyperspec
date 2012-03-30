\name{plotmat}
\alias{plotmat}
\title{Plot spectra matrix}
\usage{
  plotmat(x, ..., contour = FALSE)
}
\arguments{
  \item{x}{hyperSpec object}

  \item{...}{further parameters for
  \code{\link[graphics]{image}}}

  \item{contour}{should \code{\link[graphics]{contour}} be
  called instead of \code{\link[graphics]{image}}?}
}
\description{
  plots the spectra matrix
}
\examples{
plotmat (laser, col = alois.palette (100))
plot (laser, "mat")
plotmat (laser)
plotmat (laser, contour = TRUE, add = TRUE)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[graphics]{image}},
  \code{\link[graphics]{contour}}
}

