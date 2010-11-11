\name{Options}
\alias{hyperSpec.getOptions}
\alias{hyperSpec.setOptions}
\alias{hyperSpec.getOption}
\title{
Options for package hyperSpec
}
\description{
Functions to access and set hyperSpec's options. 
}
\usage{
hyperSpec.getOptions ()
hyperSpec.getOption (name)
hyperSpec.setOptions (\dots)
}
\arguments{
  \item{name}{the name of the option}
  \item{\dots}{pairs of argument names and values.}
}
\details{
  Currently, the following options are defined:
  \tabular{lll}{
	 \bold{Name} \tab \bold{Default Value} \tab \bold{Description} \cr
	 log \tab TRUE \tab Should \code{\link{logbook}} entries be created automatically?
  }

  \code{hyperSpec.setOptions} will discard any values that were given
  without a name.
}
\value{
  \tabular{ll}{
  \code{hyperSpec.getOptions} \tab returns a list of all options\cr
  \code{hyperSpec.setOptions} \tab invisibly returns a list with the
  options \cr
  \code{hyperSpec.getOption} \tab returns the value of the requested
  option \cr
  }
}
\author{
C. Beleites
}
\examples{
hyperSpec.getOptions ()
}
\keyword{misc}

