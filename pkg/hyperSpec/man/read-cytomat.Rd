\name{read.cytomat}
\alias{read.cytomat}
\title{Import for Cytospec mat files
These functions allow to import .mat (Matlab V5) files written by Cytospec.}
\usage{
  read.cytomat(file, keys2data = FALSE)
}
\arguments{
  \item{file}{The complete file name (or a connection to)
  the .mat file.}

  \item{keys2data}{specifies which elements of the
  \code{Info} should be transferred into the extra data}
}
\value{
  hyperSpec object if the file contains a single spectra
  block, otherwise a list with one hyperSpec object for
  each block.
}
\description{
  Import for Cytospec mat files These functions allow to
  import .mat (Matlab V5) files written by Cytospec.
}
\note{
  This function is an ad-hoc implementation and subject to
  changes.
}
\author{
  C. Beleites
}
\seealso{
  \code{R.matlab::readMat}
}
\keyword{file}
\keyword{IO}

