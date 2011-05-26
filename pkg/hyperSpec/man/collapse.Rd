\name{collapse}
\alias{collapse}
\title{Collapse hyperSpec objects}
\usage{collapse(..., short.log=TRUE, short="collapse", user, date)
}
\description{collapse/bind several hyperSpec objects into one object}
\details{The spectra from all objects will be put into one object.
The resulting object has all wavelengths that occur in the input objects.
Data points corresponding to wavelengths not in the original spectrum will be set to NA.
Extra data is combined in the same manner.\code{collapse}: The spectra from all objects will be put into one object.
The resulting object has all wavelengths that occur in the input objects.
Data points corresponding to wavelengths not in the original spectrum will be set to NA.
Extra data is combined in the same manner.


}
\author{C. Beleites}
\seealso{\code{\link[base]{merge}},  \code{\link[base]{rbind}},  \code{\link[plyr]{rbind.fill}},\code{\link[base]{merge}},  \code{\link[base]{rbind}},  \code{\link[plyr]{rbind.fill}}}
\value{a hyperSpec object\code{collapse}: a hyperSpec object
}
\keyword{manip}
\arguments{\item{...}{hyperSpec objects to be collapsed into one object. Instead of giving several
arguments, a list with all objects to be collapsed may be given.}
\item{short.log}{if \code{TRUE}, only the dimensions of the hyperSpec objects are logged}
\item{short}{handed over to \code{logentry}}
\item{user}{handed over to \code{logentry}}
\item{date}{handed over to \code{logentry}}
}
\examples{
barbiturates [1:3]
barb <- collapse (barbiturates [1:3])
barb

a <- barbiturates [[1]]
b <- barbiturates [[2]]
c <- barbiturates [[3]]

a
b
c
collapse (a, b, c)
}
\alias{collapse}
\alias{collapse.hyperSpec}
\alias{merge.hyperSpec}
\alias{rbind.fill.hyperSpec}
\keyword{manip}

