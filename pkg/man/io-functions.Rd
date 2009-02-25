\name{import}
\alias{read.txt.long}
\alias{read.txt.wide}
\alias{read.txt.Renishaw}
\alias{write.txt.long}
\alias{write.txt.wide}
\alias{import}
\alias{export}
\title{Import and Export of hyperSpec objects}
\description{
  Besides \code{\link[base]{save}} and \code{\link[base]{load}}, two
  principal ways to import and export data into \code{hyperSpec} objects
  exist.

  Firstly, the data can be exchanged via ASCII files, and the 
  Functions to import and export hyperSpec objects as ASCII
  files.

  A second option is using the package \code{\link[R.matlab]{R.matlab}}
  which provides the functions \code{\link[R.matlab]{readMat}} and
  \code{\link[R.matlab]{writeMat}}. 
}

\usage{
read.txt.long (file = stop ("filename is required"),
   cols = list (wavelength = expression (lambda / nm), spc = "I / a.u."),
   header = TRUE, \dots)

read.txt.wide (file = stop ("filename is required"),
   cols = list (spc = "I / a.u.", .wavelength = expression (lambda / nm)),
   \dots)

read.txt.Renishaw (file = stop ("filename is required"), data = "xyspc", \dots)

write.txt.long (object,  file = stop ("filename required"),
   order = NULL, na.last = TRUE, decreasing = FALSE, quote = FALSE,
   sep = "\t", row.names = FALSE, col.names = TRUE,
   col.labels = FALSE, append = FALSE, \dots)

write.txt.wide (object, file = stop ("filename required"),
   quote = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE,
   header.lines = 1, col.labels = if (header.lines == 1) FALSE else TRUE, 
   append = FALSE, \dots)
}
\arguments{
  \item{file}{filename}

  \item{cols}{}

  \item{header}{}

  \item{\dots}{}

  \item{data}{}

  \item{object}{}

  \item{order}{}

  \item{na.last}{}

  \item{decreasing}{}

  \item{quote}{}

  \item{sep}{}

  \item{row.names}{}

  \item{col.names}{}

  \item{col.labels}{}

  \item{append}{}

  \item{header.lines}{}
}
\details{
For further information, see the
  example below and the documentation of
  \code{\link[R.matlab]{R.matlab}}.
}
\examples{
## export & import matlab files
if (require (R.matlab)){
   # export to matlab file
   writeMat ("test.mat", x = flu[[]], wavelength = flu@wavelength,
             label = lapply (flu@label, as.character))

   # reading a matlab file
   data <- readMat ("test.mat")
   print (data)
   mat <- new ("hyperSpec", spc = data$x,
               wavelength = as.numeric(data$wavelength),
               label = data$label[,,1])
}

## ascii long export & import

}

\keyword{IO}
\keyword{file}