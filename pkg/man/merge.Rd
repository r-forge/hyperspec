\name{merge}
\alias{merge,hyperSpec,hyperSpec-method}
\alias{merge}
\title{Merge hyperSpec objects}
\usage{\S4method{merge}{hyperSpec,hyperSpec}(x, y, ..., short="merge", user, date)
}
\description{Merge hyperSpec objects}
\details{Merges two hyperSpec objects and cbinds their spectra matrices.
If the wavelength axis should be ordered, use \code{\link{orderwl}} afterwards.}
\author{C. Beleites}
\seealso{\code{\link[base]{merge}}
\code{\link{collapse}}, \code{\link{rbind}}, and
\code{\link{cbind}} for other ways of combining hyperSpec objects.}
\keyword{manip}
\arguments{\item{x}{a hyperSpec object}
\item{y}{a hyperSpec object}
\item{\dots}{handed to \code{\link[base]{merge.data.frame}}}
\item{short}{handed to \code{\link{logentry}}}
\item{user}{handed to \code{\link{logentry}}}
\item{date}{handed to \code{\link{logentry}}}
}
\examples{
merge (chondro [1:10,, 600], chondro [5:15,, 600], by = c("x", "y"))$.
tmp <- merge (chondro [1:10,, 610], chondro [5:15,, 610],
              by = c("x", "y"), all = TRUE)
tmp$.
wl (tmp)
\dontshow{
 stopifnot (nrow (merge (chondro [1:10], chondro [5:15], all = TRUE)) == 15)
 stopifnot (nrow (merge (chondro [1:10], chondro [5:15])) == 6)
}
}

