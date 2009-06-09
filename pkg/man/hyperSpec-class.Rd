\name{hyperSpec-class}
\docType{class}
\alias{hyperSpec-class}

\title{Class "hyperSpec"}

\description{A class for convenient handling of hyperspectral data sets,
  i.e. spectral data sets with more variates for each spectrum such as
  location, concentrations, time, etc.}

\section{hyperSpec Objects}{
  Objects can be created by calls of the form
  \code{new("hyperSpec", wavelength, data, label, log, spc, \dots)}, for
  details see \code{\link[hyperSpec]{initialize}}.
}

\section{Slots}{
	 \describe{
    \item{\code{wavelength}:}{Numeric vector giving the wavelengths
      (or wavenumbers, frequencies, m/z, \dots) for
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
    \item{write.txt.long}{\code{signature(object = "hyperSpec")}: \dots }
    \item{write.txt.wide}{\code{signature(object = "hyperSpec")}: \dots }
	 \item{read.txt.long}{\code{signature (file = "character")}: \dots }
    \item{read.txt.wide}{\code{signature(file = "character")}: \dots }
	 \item{read.txt.Renishaw}{\code{signature(object = "hyperSpec")}: \dots }
	 
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
    \item{as.matrix}{\code{signature(x = "hyperSpec")}: \dots }
	 
	 Indexing:
    \item{[}{\code{signature(x = "hyperSpec")}: \dots }
    \item{[[<-}{\code{signature(x = "hyperSpec")}: \dots }
    \item{[[}{\code{signature(x = "hyperSpec")}: \dots }
    \item{\$<-}{\code{signature(x = "hyperSpec")}: \dots }
    \item{\$}{\code{signature(x = "hyperSpec")}: \dots }
	 
	 Finding out the Dimensions:
    \item{ncol}{\code{signature(x = "hyperSpec")}: \dots }
    \item{nrow}{\code{signature(x = "hyperSpec")}: \dots }
    \item{nwl}{\code{signature(x = "hyperSpec")}: \dots }

	 
	 Apply \& Co.:
    \item{aggregate}{\code{signature(x = "hyperSpec")}:
		apply function to groups of spectra}
	 \item{apply}{\code{signature(X = "hyperSpec")}: \dots }
    \item{split}{\code{signature(x = "hyperSpec")}: \dots }
    \item{sweep}{\code{signature(x = "hyperSpec")}: \dots }
	 
	 Binding \code{hyperSpec} objects together
	 \item{cbind}{: \dots }
	 \item{rbind}{: \dots }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: \dots }
    \item{cbind2}{\code{signature(x = "hyperSpec", y = "missing")}: \dots }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "hyperSpec")}: \dots }
    \item{rbind2}{\code{signature(x = "hyperSpec", y = "missing")}: \dots }
	 \item{bind}{: \dots }

	 

	 Plotting and Printing:
    \item{plotc}{\code{signature(object = "hyperSpec")}: \dots }
    \item{plot}{\code{signature(x = "hyperSpec", y = "missing")}: \dots }
    \item{plot}{\code{signature(x = "hyperSpec", y = "character")}: \dots }
    \item{plotmap}{\code{signature(object = "hyperSpec")}: \dots }
    \item{plotspc}{\code{signature(object = "hyperSpec")}: \dots }
    \item{print}{\code{signature(x = "hyperSpec")}: \dots }
	 \item{show}{\code{signature(object = "hyperSpec")}: \dots }
	 
	 
    \item{decompose}{\code{signature(x = "hyperSpec")}: \dots }
    \item{logentry}{\code{signature(x = "hyperSpec")}: \dots }

	 Spectra-related:
    \item{wli}{\code{signature(x = "hyperSpec")}: \dots }
	 \item{orderwl}{\code{signature(x = "hyperSpec")}: \dots }
	 \item{spc.bin}{\code{signature(hyperSpec = "hyperSpec")}: \dots }
    \item{spc.fit.poly.below}{\code{signature(fit = "hyperSpec")}: \dots }
    \item{spc.fit.poly}{\code{signature(fit = "hyperSpec")}: \dots }
	 
	 }
}


\references{ ~put references to the literature/web site here ~ }
\author{Claudia Beleites \email{cbeleites@units.it}}
\note{ ~~further notes~~ }

\section{Warning}{\dots.}

\examples{
showClass("hyperSpec")
}
\keyword{classes}
