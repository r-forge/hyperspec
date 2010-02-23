\name{axis.break}
\alias{axis.break}
\title{Poor man's version of plotrix' axis.break}
\description{
If package plotrix is not available, \code{\link{plotspc}} uses this
function to place cutmarks on axes.
}
\usage{
axis.break(axis = 1, breakpos = NULL, ...)
}
\arguments{
  \item{axis}{which axis to use?}
  \item{breakpos}{Where should the mark go}
  \item{\dots}{ignored}
}
\value{
an invisible \code{NULL}
}
\author{
C. Beleites
}

\seealso{
\code{\link[plotrix]{axis.break}}
}
\examples{
plot (flu)
axis.break (breakpos = 445)
}
\keyword{aplot}
