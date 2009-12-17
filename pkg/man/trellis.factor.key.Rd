\name{trellis.factor.key}
\alias{trellis.factor.key}
\title{
Color coding legend for factors
}
\description{
Modifies a list of lattice arguments (as for
\code{\link[lattice]{levelplot}}, etc.) according to the factor levels.
The colorkey will shows all levels (including unused), and the drawing colors will be set accordingly.
}
\usage{
trellis.factor.key(f, levelplot.args = list())
}
\arguments{
  \item{f}{
	 the factor that will be color-coded
}
  \item{levelplot.args}{
	 a list with levelplot arguments
}
}
\details{\code{trellis.factor.key} is used during
  \code{levelplot}-based plotting of factors (for hyperSpec objects) unless
  \code{transform.factor = FALSE} is specified.}
\value{
the modified list with levelplot arguments.
}
\author{C. Beleites}

\seealso{\code{\link[lattice]{levelplot}}}
\examples{
chondro$z <- factor (rep (c("a", "a", "d", "c"),
                          length.out = nrow (chondro)),
                     levels = letters [1 : 4])

str (trellis.factor.key (chondro$z))

plotmap (chondro, z ~ x * y)

## switch off using trellis.factor.key:
## note that the factor levels are collapsed to c(1, 2, 3) rather than
## c (1, 3, 4)
plotmap (chondro, z ~ x * y, transform.factor = FALSE)

plotmap (chondro, z ~ x * y,
         col.regions = c ("gray", "red", "blue", "dark green"))
}
\keyword{aplot}

