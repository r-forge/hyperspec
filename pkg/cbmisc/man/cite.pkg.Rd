\name{cite.pkg}
\alias{cite.pkg}
\title{cite a package}
\usage{
  cite.pkg(pkg, entries, citefun = "cite")
}
\arguments{
  \item{pkg}{package}

  \item{entries}{which \code{citation ()} entries to cite?}

  \item{citefun}{which LaTeX command to use for the
  citiation.}
}
\value{
  character with \code{\\cite{pkg.1, pkg.2}}
}
\description{
  cite a package
}
\author{
  Claudia Beleites
}

