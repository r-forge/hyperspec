\name{kernelpls.fit}
\alias{kernelpls.fit}
\title{PLS fitting}
\usage{
  kernelpls.fit(X, Y, ncomp, stripped = FALSE, ...,
    center.x = center, center.y = center, center = TRUE)
}
\arguments{
  \item{X,Y,ncomp,stripped,...}{see
  \code{\link[pls]{kernelpls.fit}}}

  \item{center}{centering as used by \code{\link{scale}}}

  \item{center.x,center.y}{like \code{center}, but specify
  different treatment for \code{X} and \code{Y}}
}
\description{
  PLS fitting functions from pacakge pls, enhanced so that
  custom centering can be given.
}
\seealso{
  \code{\link[pls]{kernelpls.fit}}
}

