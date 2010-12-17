\name{Options}
\alias{hyperSpec.getOptions}
\alias{hyperSpec.setOptions}
\alias{hyperSpec.getOption}
\alias{hyperSpec options}
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
  \tabular{llll}{
	 \bold{Name} 		  \tab \bold{Default Value (range)} \tab	 \bold{Description} 							 \tab \bold{Used by}\cr
	 debuglevel 		  \tab 0 (1L 2L) 							\tab amount of debugging information produced \tab \code{\link{spc.identify}} \cr
	 gc 					  \tab FALSE 								\tab triggers frequent calling of gc () 		 \tab \code{\link{read.ENVI}}, \code{new ("hyperSpec")}\cr
	 log 					  \tab TRUE 								\tab automatically create entries 				 \tab \code{\link{logbook}}\cr
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

