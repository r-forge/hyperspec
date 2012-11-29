\name{read.jdx}
\alias{read.jdx}
\alias{read.jdx.Shimadzu}
\title{JCAMP-DX Import for Shimadzu Library Spectra}
\usage{
  read.jdx(filename = stop("filename is needed"),
    encoding = "", header = list(), keys.hdr2data = FALSE,
    ...)

  read.jdx.Shimadzu(filename, encoding = "")
}
\arguments{
  \item{filename}{file name and path of the .jdx file}

  \item{encoding}{encoding of the JCAMP-DX file (used by
  \code{\link[base]{readLines}})}

  \item{header}{list with manually set header values}

  \item{keys.hdr2data}{character vector of labels
  (lowercase, without and dashes, blanks, underscores)
  whose values should be tranfered into the extra data.}

  \item{\dots}{further parameters handed to the data import
  function, e.g. \tabular{ll}{ \code{xtol} \tab tolerance
  for checking calculated x values against checkpoints at
  beginning of line, defaults to XFACTOR\cr \code{ytol}
  \tab tolerance for checking Y values against MINY and
  MAXY, defaults to YFACTOR\cr }}
}
\value{
  hyperSpec object
}
\description{
  this is a first rough import function for JCAMP-DX
  spectra as exported by the Shimadzu GCxGC-MS library
}
\note{
  JCAMP-DX support is incomplete and the functions may
  change without notice. See \code{vignette ("fileio")}
}
\author{
  C. Beleites
}

