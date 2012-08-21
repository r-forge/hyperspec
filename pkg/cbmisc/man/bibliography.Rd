\name{citation.or.file}
\alias{citation.or.file}
\alias{citation.with.key}
\alias{cite.pkg}
\alias{make.bib}
\title{retrieve citation object with fallback to saved citations.}
\usage{
  citation.or.file(pkg,
    saved.cit = sprintf("\%s.CITATION", pkg))

  citation.with.key(pkg = "base")

  cite.pkg(pkg, entries, citefun = "cite")

  make.bib(..., file = NULL)
}
\arguments{
  \item{pkg}{

  package

  package}

  \item{saved.cit}{CITATION file on harddisk}

  \item{entries}{which \code{citation ()} entries to cite?}

  \item{citefun}{which LaTeX command to use for the
  citiation.}

  \item{...}{packages}

  \item{file}{name of .bib file \code{NULL} suppresses
  writing of the file. \code{""} will output to stdout.}
}
\value{
  citation

  citation

  character with \code{\\cite{pkg.1, pkg.2}}

  invisible bibentry list.stats
}
\description{
  retrieves \code{citation (pkg)} if package pkg is
  available. Alternatively, a CITATION file can be read.

  citations with automatic BibTeX keys

  cite a package

  Produce BibTeX file including automatic keys.
}
\examples{
cite.pkg ("cbmisc")
make.bib (file = "")
}
\author{
  Claudia Beleites

  Claudia Beleites

  Claudia Beleites

  Claudia Beleites
}

