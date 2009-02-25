\name{apply-methods}
\docType{methods}
%\alias{apply}
%\alias{apply-methods}
\alias{apply,hyperSpec-method}
\title{apply}
\description{
 Computes summary statistics for a \code{hyperSpec}
  object. 
}
\usage{
\S4method{apply}{hyperSpec}(X, MARGIN, FUN, ...,
   short = NULL, user = NULL, date = NULL)
%apply (X, MARGIN, FUN, ..., short = NULL, user = NULL, date = NULL)
}
\arguments{
  \item{X}{a \code{hyperSpec} object}
  \item{MARGIN}{The subscript which the function will be
    applied over. \code{1} indicates rows, \code{2} indicates columns,
	 \code{1:2} indicates that \code{FUN} should be applied to all
	 elements of the spectra matrix.
    }
  \item{FUN}{function to compute the summary statistics}
  \item{\dots}{further arguments passed to \code{FUN}}
  \item{short, user, date}{aguments passed to \code{logentry}}
}
\value{
  A \code{hyperSpec} object
}

\details{
   \code{apply} gives the functionality of
	\code{\link[stats]{apply}} for \code{hyperSpec} objects.

  The generic functions of group \code{\link[methods]{Math}} are not
  definded for \code{hyperSpec} objects. Instead, \code{apply} can be
  used. For functions like \code{log} that work on scalars,
  \code{MARGIN = 1 : 2} gives the appropriate behaviour.
}
\seealso{
  \code{\link[stats]{apply}}, for applying \code{FUN} to subgroups of
  the \code{hyperSpec} object:  \code{\link{aggregate}}.
}
\examples{

plot (apply (chondro, 2, range), "spc")

plot (apply (chondro, 1, mean), "map")

flu[[,,405:407]]
apply (flu, 1:2, "*", -1)[[,,405:407]]

all (apply (flu, 1:2, log)[[]] - log(flu[[]]) == 0)
}

\author{C. Beleites}

\keyword{methods}


