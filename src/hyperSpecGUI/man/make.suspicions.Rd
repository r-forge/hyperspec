\name{make.suspicions}
\alias{make.suspicions}
\title{extract suspicions from spikiness matrix...}
\usage{make.suspicions(spikiness, condition=spikiness > quantile(spikiness, 0.975))
}
\description{extract suspicions from spikiness matrix}
\value{matrix nsuspicions x 3. The colums hold the row (spectra) index, wavelength index, and
the score. Rows are sorted with descending spikiness.}
\author{Claudia Beleites}
\arguments{\item{spikiness}{matrix indicating how suspicious each point is}
\item{condition}{filtering condition indication which parts of \code{spikiness} should be
considered.}
}

