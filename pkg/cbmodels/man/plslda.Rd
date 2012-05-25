\name{plslda}
\alias{plslda}
\alias{predict.plslda}
\title{PLS-LDA}
\usage{
  plslda(X, Y, grouping, ...)

  predict.plslda(object, newdata, ...)
}
\arguments{
  \item{X}{input variate matrix}

  \item{Y}{matrix with class membership (classes correspond
  to columns).  If missing,
  \code{\link[softclassval]{factor2matrix} (grouping)} is
  used.}

  \item{grouping}{factor with class membership. If missing,
  \code{\link[softclassval]{hardclasses} (Y)} is used.}

  \item{...}{further parameters for
  \code{\link[pls]{plsr}}, importantly, the number of PLS
  variates to be used can be given via \code{ncomp}.

  further arguments for \code{link[MASS]{predict.lda}} and
  \code{link[pls]{predict.mvr}}}

  \item{object}{the plslda model}

  \item{newdata}{matrix with new cases to be predicted.}
}
\value{
  object of class "plslda", consisting of the mvr object
  returned by \code{\link[pls]{plsr}} and the lda object
  returned by \code{\link[MASS]{lda}}.

  list with results from \code{link[MASS]{predict.lda}}
  plus the pls scores used for LDA prediction in element
  \code{$scores}
}
\description{
  LDA models using PLS latent variables as input space.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[pls]{plsr}}, \code{\link[MASS]{lda}}

  \code{link[MASS]{predict.lda}},
  \code{link[pls]{predict.mvr}}
}

