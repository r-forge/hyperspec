\name{hyperSpec-class}
\docType{class}
\alias{hyperSpec-class}

\title{Class "hyperSpec"}

\description{A class for convenient handling of hyperspectral data sets,
  i.e. spectral data sets with more variates for each spectrum such as
  location, concentrations, time, etc.}

\section{hyperSpec Objects}{
  Objects can be created by calls of the form
  \code{new("hyperSpec", wavelength, data, label, log, spc, ...)}, for
  details see \code{\link[hyperSpec]{initialize}}.
}

\section{Slots}{
	 \describe{
    \item{\code{wavelength}:}{Numeric vector giving the wavelengths
      (or wavenumbers, frequencies, m/z, ...) for
      each data point of the spectrum.
	
		\code{wavelength} wavelength axis of the spectra.
	 }
    \item{\code{data}:}{\code{data.frame} with the spectra and further
      data for each spectrum (e.g. x/y/z coordinates, times, sample
      numbers, concentrations, etc.).
		
      The spectra are stored in \code{data$spc}, preferrably as a matrix.
    }
    \item{\code{label}:}{List with the labels (character vectors or
		expressions) that should be used to describe the columns of
		\code{data}. 

		The label for \code{wavelength} is in \code{label$.wavelength}.
	 }
    \item{\code{log}:}{A \code{data.frame} with the columns
      \code{short.description}, \code{long.description}, \code{date},
      and \code{name} tracking what is done with the object.}
  }
}

\section{Methods}{
  \describe{
	 Construction
    \item{initialize}{\code{signature(.Object = "hyperSpec")}: see
		\code{\link[hyperSpec]{initialize}}}

	 Loading and Saving ASCII Data: \link{import}
    \item{write.txt.long}{\code{signature(object = "hyperSpec")}: ... }
    \item{write.txt.wide}{\code{signature(object = "hyperSpec")}: ... }
	 \item{read.txt.long}{\code{signature (file = "character")}: ... }
    \item{read.txt.wide}{\code{signature(file = "character")}: ... }
	 \item{read.txt.Renishaw}{\code{signature(object = "hyperSpec")}: ... }
	 
	 Arithmetical Operators:  "+", "-", "*", "\^", "\%\%", "\%/\%", "/"
	 
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "hyperSpec")}}
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "numeric")}}
    \item{Arith}{\code{signature(e1 = "hyperSpec", e2 = "matrix")}}
    \item{Arith}{\code{signature(e1 = "numeric", e2 = "hyperSpec")}}
    \item{Arith}{\code{signature(e1 = "matrix", e2 = "hyperSpec")}}
	 see \code{\link{Arith}}

	 Conversion:
    \item{\link{as.character}}{\code{signature(x = "hyperSpec")}}
    \item{as.data.frame}{\code{signature(x = "hyperSpec", row.names = "missing", optional = "missing")}}
    \item{as.matrix}{\code{signature(x = "hyperSpec")}: ... }
	 
	 Indexing:
    \item{[}{\code{signature(x = "hyperSpec")}: ... }
    \item{[[<-}{\code{signature(x = "hyperSpec")}: ... }
    \item{[[}{\code{signature(x = "hyperSpec")}: ... }
    \item{\$<-}{\code{signature(x = "hyperSpec")}: ... }
    \item{\$}{\code{signature(x = "hyperSpec")}: ... }
	 
	 Finding out the Dimensions:
    \item{ncol}{\code{signature(x = "hyperSpec")}: ... }
    \item{nrow}{\code{signature(x = "hyperSpec")}: ... }
    \item{nwl}{\code{signature(x = "hyperSpec")}: ... }

	 
	 Apply \& Co.:
    \item{aggregate}{\code{signature(x = "hyperSpec")}:
		apply function to groups of spectra}
	 \item{apply}{\code{signature(X = "hyperSpec")}: ... }
    \item{split}{\code{signature(x = "hyperSpec")}: ... }
    \item{sweep}{\code{signature(x = "hyperSpec")}: ... }
	 
	 Binding \code{hyperSpec} objects together
	 \item{cbind}{: ... }
	 \item{rbind}{: ... }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: ... }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: ... }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
	 \item{bind}{: ... }

	 

	 Plotting and Printing:
    \item{plotc}{\code{signature(object = "hyperSpec")}: ... }
    \item{plot}{\code{signature(x = "hyperSpec", y = "missing")}: ... }
    \item{plot}{\code{signature(x = "hyperSpec", y = "character")}: ... }
    \item{plotmap}{\code{signature(object = "hyperSpec")}: ... }
    \item{plotspc}{\code{signature(object = "hyperSpec")}: ... }
    \item{print}{\code{signature(x = "hyperSpec")}: ... }
	 \item{show}{\code{signature(object = "hyperSpec")}: ... }
	 
	 
    \item{decompose}{\code{signature(x = "hyperSpec")}: ... }
    \item{logentry}{\code{signature(x = "hyperSpec")}: ... }

	 Spectra-related:
    \item{wli}{\code{signature(x = "hyperSpec")}: ... }
	 \item{orderwl}{\code{signature(x = "hyperSpec")}: ... }
	 \item{spc.bin}{\code{signature(hyperSpec = "hyperSpec")}: ... }
    \item{spc.fit.poly.below}{\code{signature(fit = "hyperSpec")}: ... }
    \item{spc.fit.poly}{\code{signature(fit = "hyperSpec")}: ... }
	 
	 }
}


\references{ ~put references to the literature/web site here ~ }
\author{Claudia Beleites \email{cbeleites@units.it}}
\note{ ~~further notes~~ }

\section{Warning}{....}

\examples{
showClass("hyperSpec")
}
\keyword{classes}
