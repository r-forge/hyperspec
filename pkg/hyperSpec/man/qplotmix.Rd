\name{qplotmix}
\alias{legendright}
\title{Plot multivariate data into colour channels...}
\usage{legendright(p, l, legend.width=8, legend.unit="lines")

qmixtile(object, purecol=stop("pure component colors needed."),
    mapping=aes_string(x = "x", y = "y", fill = "spc"), ...)


normalize.colrange(x, na.rm=TRUE, legend=FALSE, n=100)


normalize.range(x, na.rm=TRUE, legend=FALSE, n=100)


normalize.minmax(x, min=0, max=1, legend=FALSE, n=100)


qmixlegend(x, purecol, dx=0.33, ny=100, labels=names(purecol),
    normalize=normalize.colrange, ...)


colmix.rgb(x, purecol, against=1, sub=TRUE, normalize=normalize.colrange, ...)

}
\description{Plot multivariate data into colour channels}
\details{\code{legendright}: plot graph with legend right of it

}
\value{\code{legendright}: invisible \code{NULL}

\code{normalize.colrange}: list with components ymin, max and fill to specify value and fill colour value (still
numeric!) for the legend, otherwise the normalized values


\code{qmixlegend}: ggplot object with legend


\code{colmix.rgb}: character with colours


}
\author{Claudia Beleites}
\arguments{\item{p}{plot object}
\item{l}{legend object}
\item{legend.width,legend.unit}{size of legend part}
\item{object}{matrix to be plotted with mixed colour channels}
\item{purecol}{pure component colours, names determine legend labels}
\item{mapping}{see \code{\link[ggplot2]{geom_tile}}}
\item{...}{handed to \code{colmix.rgb}}
\item{x}{numeric matrix}
\item{na.rm}{see \code{link[base]{min}}}
\item{legend}{should a legend be produced instead of normalized values?}
\item{n}{of colours to produce in legend}
\item{min}{numeric with value corresponding to "lowest" colour for each column}
\item{max}{numeric with value corresponding to "hightest" colour for each column}
\item{dx}{width of label bar}
\item{ny}{number of colours in legend}
\item{labels}{component names}
\item{against}{value to mix against (for \code{sub = TRUE} only, 1 = white, 0 = black)}
\item{sub}{subtractive color mixing?}
\item{normalize}{function to normalize the values.}
}
\alias{qmixtile}
\alias{normalize.colrange}
\alias{normalize.range}
\alias{normalize.minmax}
\alias{qmixlegend}
\alias{colmix.rgb}

