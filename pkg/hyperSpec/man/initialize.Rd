% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/initialize.R
\docType{methods}
\name{initialize}
\alias{create}
\alias{create,hyperSpec-method}
\alias{initialize}
\alias{initialize,hyperSpec-method}
\alias{new}
\alias{new,hyperSpec-method}
\title{Creating a hyperSpec Object
Like other S4 objects, a hyperSpec object can be created by \code{new}. The
hyperSpec object is then \code{initialize}d using the given parameters.}
\usage{
\S4method{initialize}{hyperSpec}(.Object, spc = NULL, data = NULL,
  wavelength = NULL, labels = NULL)
}
\arguments{
\item{.Object}{the new \code{hyperSpec} object.}

\item{spc}{the spectra matrix.

\code{spc} does not need to be a matrix, it is converted explicitly by
  \code{I (as.matrix (spc))}.}

\item{data}{\code{data.frame}, possibly with the spectra in
  \code{data$spc}, and further variates in more columns.  A matrix can be
  entered as \emph{one} column of a data frame by: \code{data.frame (spc =
  I (as.matrix (spc)))}.

However, it will usually be more convenient if the spectra are given in
  \code{spc}}

\item{wavelength}{The wavelengths corresponding to the columns of
\code{data}. If no wavelengths are given, an appropriate vector is
derived from the column names of \code{data$spc}. If this is not
possible, \code{1 : ncol (data$spc)} is used instead.}

\item{labels}{A \code{list} containing the labels for the columns of the
  \code{data} slot of the \code{hyperSpec} object and for the wavelength
  (in \code{label$.wavelength}). The labels should be given in a form ready
  for the text-drawing functions (see \code{\link[grDevices]{plotmath}}).

If \code{label} is not given, a list containing \code{NULL} for each of the
  columns of\code{data} and \code{wavelength} is used.}
}
\description{
If option \code{gc} is \code{TRUE}, the initialization will have frequent
calls to \code{gc ()} which can help to avoid swapping or running out of
memory.
}
\examples{
new ("hyperSpec")

spc <- matrix (rnorm (12), ncol = 4)
new ("hyperSpec", spc = spc)
new ("hyperSpec", data = data.frame (x = letters[1:3]),
     spc = spc)

colnames (spc) <- 600:603
new ("hyperSpec", spc = spc)  # wavelength taken from colnames (spc)

# given wavelengths precede over colnames of spc
new ("hyperSpec", spc = spc, wavelength = 700:703)

# specifying labels
h <- new ("hyperSpec", spc = spc, data = data.frame (pos = 1 : 3),
          label = list (spc = "I / a.u.",
                        .wavelength = expression (tilde (nu) / cm^-1),
                        pos = expression ("/" (x, mu*m)))
)

plot (h)
plotc (h, spc ~ pos)
}
\author{
C.Beleites
}
\seealso{
\code{\link[methods]{new}} for more information on creating and
  initializing S4 objects.

\code{\link[grDevices]{plotmath}} on expressions for math annotations as
  for slot \code{label}.

\code{\link{hy.setOptions}}
}
\keyword{datagen}
\keyword{methods}

