% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/options.R
\name{hy.getOptions}
\alias{hy.getOption}
\alias{hy.getOptions}
\alias{hy.setOptions}
\title{Options for package hyperSpec
Functions to access and set hyperSpec's options.}
\usage{
hy.getOptions(...)

hy.getOption(name)

hy.setOptions(...)
}
\arguments{
\item{...}{\code{hy.setOptions}: pairs of argument names and values.

\code{hy.getOptions}: indices (or names) of the options.}

\item{name}{the name of the option}
}
\value{
\tabular{ll}{
\code{hy.getOptions} \tab returns a list of all options\cr
\code{hy.setOptions} \tab invisibly returns a list with the options \cr
\code{hy.getOption}  \tab returns the value of the requested option \cr
}
}
\description{
Currently, the following options are defined:
\tabular{llll}{
\bold{Name}          \tab \bold{Default Value (range)}      \tab \bold{Description}                               \tab \bold{Used by}\cr
debuglevel           \tab 0 (1L 2L)                         \tab amount of debugging information produced         \tab \code{\link{spc.identify}} \code{\link{map.identify}}\cr
                     \tab                                   \tab                                                  \tab various file import functions\cr
gc                   \tab FALSE                             \tab triggers frequent calling of gc ()               \tab \code{\link{read.ENVI}}, \code{new ("hyperSpec")}\cr
file.remove.emptyspc \tab TRUE                              \tab remove empty spectra directly on file import     \tab various file import functions\cr
file.keep.name       \tab TRUE                              \tab always create filename column                    \tab various file import functions\cr
tolerance            \tab \code{sqrt (.Machine$double.eps)} \tab tolerance for numerical comparisons              \tab \code{\link{normalize01}}, file import: \code{file.remove.emptyspc}\cr
wl.tolerance         \tab \code{sqrt (.Machine$double.eps)} \tab tolerance for comparisons of the wavelength axis \tab \code{\link{all.equal}}, \code{\link{collapse}}, \code{\link{rbind}}\cr
}
}
\details{
\code{hy.setOptions} will discard any values that were given without a
name.
}
\examples{
hy.getOptions ()
}
\author{
C. Beleites
}
\keyword{misc}

