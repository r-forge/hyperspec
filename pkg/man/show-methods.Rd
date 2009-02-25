\name{show-methods}
\docType{methods}
\alias{show}
\alias{show-methods}
\alias{show,hyperSpec-method}
\alias{print}
\alias{print-methods}
\alias{print,hyperSpec-method}
\alias{as.character}
\alias{as.character,hyperSpec-method}
\alias{as.character-methods}
\title{ Convert a hyerSpec object to character strings for Display}
\description{
  \code{print} and \code{show} show the result of \code{as.character}
}
\usage{
\S4method{show}{hyperSpec}(x)
\S4method{print}{hyperSpec}(x, \dots)
\S4method{as.character}{hyperSpec}(x, digits = getOption ("digits"), max.print = 25, shorten.to = c(3,3))
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  
  \item{digits}{number of digits handed over to \code{format}}
  
  \item{max.print}{maximum number of elements to be printed (of a
  variable)}
  
  \item{shorten.to}{if a vector is longer than \code{max.print}, only
    the first \code{shorten.to[1]} and the last \code{shorten.to[2]}
    elements are printed}

  \item{\dots}{\code{print} hands any arguments to \code{as.character}}
}
\value{
  \code{as.character} returns a character vector fit to be printed by
  \code{cat} with \code{sep = "\\n"}.

  \code{print} invisibly returns \code{x} after printing, \code{show}
  returns an invisible \code{NULL}.
  }

\seealso{
  \code{\link[base]{print}},
  \code{\link[methods]{show}}, and
  \code{\link[base]{as.character}}
}
\examples{
chondro
}  
\keyword{methods}
\keyword{print}
