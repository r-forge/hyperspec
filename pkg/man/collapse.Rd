\name{collapse}
\alias{collapse}
\title{Collapse hyperSpec objects}
\usage{collapse(..., orderwl=TRUE, short.log=TRUE, short="collapse", user, date)
}
\description{collapse/bind several hyperSpec objects into one object}
\details{The spectra from all objects will be put into one object.
The resulting object has all wavelengths that occur in the input objects.
Data points corresponding to wavelengths not in the original spectrum will be set to NA.
Extra data is combined in the same manner.}
\author{C. Beleites}
\alias{collapse}
\alias{collapse.hyperSpec}
\alias{merge.hyperSpec}
\alias{rbind.fill.hyperSpec}
\seealso{\code{\link[base]{merge}},  \code{\link[base]{rbind}},  \code{\link[plyr]{rbind.fill}},}
\value{a hyperSpec object}
\keyword{manip}
\arguments{\item{...}{hyperSpec objects to be collapsed into one object. Instead of giving several
arguments, a list with all objects to be collapsed may be given.}
\item{orderwl}{}
\item{short,user,date}{handed to logentry}
}
\examples{
barbituates [1:3]

barb <- collapse (barbituates [1:3])
barb

a <- barbituates [[1]]
b <- barbituates [[2]]
c <- barbituates [[3]]
a
b
c
collapse (a, b, c)

}

