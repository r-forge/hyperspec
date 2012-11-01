\name{plslda}
\alias{center.plslda}
\alias{coef.plslda}
\alias{plslda}
\alias{predict.plslda}
\title{PLS-LDA}
\usage{
  plslda(X, Y, grouping, comps = TRUE, ncomp = min(dim(X)),
    stripped = TRUE, ...)

  \method{predict}{plslda} (object, newdata, ...)

  \method{coef}{plslda} (object, ...)

  \method{center}{plslda} (object, ...)
}
\arguments{
  \item{X}{input variate matrix}

  \item{Y}{matrix with class membership (classes correspond
  to columns).  If missing,
  \code{\link[softclassval]{factor2matrix} (grouping)} is
  used.}

  \item{grouping}{factor with class membership. If missing,
  \code{\link[softclassval]{hardclasses} (Y)} is used.}

  \item{comps}{which latent variables should be used?}

  \item{ncomp}{how many latent variables should be
  calculated?}

  \item{stripped}{should the model be stripped to save
  memory? (Stripping is different from
  \code{\link[pls]{plsr}}'s stripping.)}

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
\details{
  For the moment only \code{\link{kernelpls.fit}} (see
  \code{\link[pls]{kernelpls.fit}} for the original) is
  supported.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[pls]{plsr}}, \code{\link[MASS]{lda}}

  \code{\link[MASS]{predict.lda}},
  \code{\link[pls]{predict.mvr}}
  \code{\link[cbmodels]{rotate}} for rotation of the LDA
  part of the model
}

