\name{citation.or.file}
\alias{citation.or.file}
\title{retrieve citation object with fallback to saved citations.}
\usage{
  citation.or.file(pkg,
    saved.cit = sprintf("\%s.CITATION", pkg))
}
\arguments{
  \item{pkg}{}

  \item{saved.cit}{CITATION file on harddisk}
}
\value{
  citation
}
\description{
  retrieves \code{citation (pkg)} if package pkg is
  available. Alternatively, a CITATION file can be read.
}
\author{
  Claudia Beleites
}

