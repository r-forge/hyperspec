\name{pcaadd}
\alias{pcaadd}
\alias{pcaadd,hyperSpec,hyperSpec-method}
\alias{pcaadd,hyperSpec,matrix-method}
\alias{pcaadd,matrix,hyperSpec-method}
\alias{pcaadd,matrix,matrix-method}
\alias{predict.pcaadd}
\title{Principal component analysis with reference spectra}
\usage{
  \S4method{pcaadd}{matrix,matrix}(x, reference, ...,
    ref.factor = 10, refcomps = seq_len(nrow(reference)))

  \S4method{pcaadd}{hyperSpec,hyperSpec}(x, reference, ...)

  \S4method{pcaadd}{hyperSpec,matrix}(x, reference, ...)

  \S4method{pcaadd}{matrix,hyperSpec}(x, reference, ...)

  \method{predict}{pcaadd} (object, newdata, ...)
}
\arguments{
  \item{x}{data matrix}

  \item{reference}{reference data points}

  \item{...}{further arguments are handed to prcomp, but
  \code{center} is always \code{FALSE}.}

  \item{ref.factor}{reference is multiplied by
  \code{ref.factor * max (abs (range (x))) / max (abs
  (range (reference))) * nrow (x) / nrow (reference)}
  before the PCA. This forces the first directions towards
  \code{reference}.}

  \item{refcomps}{the principal components that are
  attributed to the reference}

  \item{object}{pcaadd model}

  \item{newdata}{matrix with new observations.}
}
\value{
  object of class "pcaadd"
}
\description{
  A PCA model is calculated after a multiple of the
  reference matrix is added to the data matrix.
}

