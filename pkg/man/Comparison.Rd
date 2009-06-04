\name{Cmparison Operators}
\docType{methods}
\alias{hyperSpec Comparison}
\alias{hyperSpec compare}
\alias{hyperSpec Comparison Operators}
\alias{Compare,hyperSpec-method}
\alias{Compare,hyperSpec,hyperSpec-method}
\alias{<,hyperSpec,hyperSpec-method}
\alias{>,hyperSpec,hyperSpec-method}
\alias{<=,hyperSpec,hyperSpec-method}
\alias{>=,hyperSpec,hyperSpec-method}
\alias{==,hyperSpec,hyperSpec-method}
\alias{!=,hyperSpec,hyperSpec-method}
\alias{Compare,hyperSpec,matrix-method} 
\alias{Compare,hyperSpec,numeric-method}
\alias{Compare,matrix,hyperSpec-method}
\alias{Compare,numeric,hyperSpec-method}
\title{Comparison Operators: <, >, <=, >=, ==, and !=}
\description{The comparison operators \code{">"}, \code{"<"},
  \code{">="}, \code{"<="}, \code{"=="}, and \code{"!="} 
  for \code{hyperSpec} objects. 
\code{
#S4 method for signature 'hyperSpec,hyperSpec':
#S4 method for signature 'hyperSpec,matrix':
#S4 method for signature 'matrix,hyperSpec':
#S4 method for signature 'hyperSpec,numeric':
#S4 method for signature 'numeric,hyperSpec':
e1 > e2
`>` (e1, e2)
e1 < e2
e1 >= e2
e1 <= e2
e1 == e2
e1 != e2
}
}
\arguments{
  \item{e1, e2}{Either two \code{hyperSpec} objects or one
	 \code{hyperSpec} object and  matrix of same size
    as \code{hyperSpec[[]]} or a scalar (numeric of length 1).
  } 
}
\details{
   The comparison operators \code{">"}, \code{"<"},
  \code{">="}, \code{"<="}, \code{"=="}, and \code{"!="} work on the 
  spectra matrix of the \code{hyperSpec} object. They have their
  usual meaning (see \code{\link[base]{Comparison}}).
  The operators work also with one \code{hyperSpec} object and a numeric (scalar) object
  or a matrices of the same size as the spectra matrix of the \code{hyperSpec}
  object.

  With numeric vectors \code{\link[hyperSpec]{sweep}} might be more appropriate.
  
  If you want to calculate on the \code{data.frame} \code{hyperSpec@data}, you
  have to do this directly on \code{hyperSpec@data}.
}

\value{a logical matrix

As code{hyperSpec} objects must have numeric spectra matrices, 
the resulting matrix of the comparison is returned directly.

This also allows indexing 
}

\author{C. Beleites}

\examples{

}
\seealso{
  \code{\link[hyperSpec]{sweep-methods}} for calculations involving a vector and
  the spectral matrix.
  
  \code{\link[methods]{S4groupGeneric}} for group generic methods.

  \code{\link[base]{Comparison}} for the base comparison functions.
  
  \code{\link[hyperSpec]{Arith}} for arithmetic operators, 
  \code{\link[hyperSpec]{Math}} and \code{\link[hyperSpec]{Math2}} for mathematical group generic 
  functions working on \code{hyperSpec} objects.  
}
\keyword{methods}
\keyword{compare}
