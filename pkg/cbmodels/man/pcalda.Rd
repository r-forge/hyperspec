\name{pcalda}
\alias{center.pcalda}
\alias{coef.pcalda}
\alias{pcalda}
\alias{predict.pcalda}
\title{PCA-LDA}
\usage{
  pcalda(X, Y, grouping, comps = TRUE, ...)

  \method{predict}{pcalda} (object, newdata, ...)

  \method{coef}{pcalda} (object, ...)

  \method{center}{pcalda} (object, ...)
}
\arguments{
  \item{X}{input variate matrix}

  \item{Y}{matrix with class membership (classes correspond
  to columns).  If missing,
  \code{\link[softclassval]{factor2matrix} (grouping)} is
  used.}

  \item{grouping}{factor with class membership. If missing,
  \code{\link[softclassval]{hardclasses} (Y)} is used.}

  \item{...}{handed to \code{\link[stats]{prcomp}} and
  \code{\link[MASS]{lda}} (e.g. \code{subset})
  \code{\link[MASS]{lda}}.

  \code{predict} ignores further arguments}

  \item{comps}{which principal components should be used?}

  \item{object}{the PCA-LDA model}

  \item{newdata}{the new data to apply the model to
  (matrix)}
}
\value{
  object of class "pcalda", consisting of the prcomp object
  returned by \code{\link[stats]{prcomp}} and the lda
  object returned by \code{\link[MASS]{lda}}.
}
\description{
  LDA models using principle components as input space.
}
\examples{
chondro <- chondro [! is.na (chondro$clusters)]
model <- pcalda (X = chondro[[]], grouping = chondro$clusters, comps = 1 : 3)

names (model)

pred <- predict (model)

plot
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[stats]{prcomp}}, \code{\link[MASS]{lda}}
}

