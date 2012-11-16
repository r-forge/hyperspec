\name{flip}
\alias{flip}
\alias{flip.lda}
\alias{flip.pcalda}
\alias{flip.plslda}
\title{flip}
\usage{
  flip(object, dims, ...)

  \method{flip}{lda} (object, dims = FALSE, ...)

  \method{flip}{pcalda} (object, dims = FALSE, ...)

  \method{flip}{plslda} (object, dims = FALSE, ...)
}
\arguments{
  \item{object}{a model to flip}

  \item{dims}{dimensions which should be mirrored}

  \item{...}{further parameters}
}
\value{
  the flipped/mirrored model
}
\description{
  Flipping of models

  \code{\link[MASS]{lda}} models are invariant to flipping,
  i.e. the predicted posterior probabilities and classes do
  not change if the LD scores \code{\link[MASS]{lda}$x} are
  multiplied by -1.
}
\details{
  \code{flip} is a post-processing method for models. It
  flips (mirrors) dimensions of the model space.
}
\examples{
## flip 1st dimension
model <- lda (Species ~ ., data = iris)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.flip <- flip (model, 1)
pred.flip <- predict (model.flip)
points (pred.flip$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and rotated model's predictions
diff <- pred$posterior - pred.flip$posterior
summary (diff)
boxplot (diff)
## mirror x axis
model <- pcalda (X = iris [,1:4], grouping = iris$Species, comps=1:3)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.flip <- flip (model, 1)
pred.flip <- predict (model.flip)
points (pred.flip$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and flipped model's predictions
diff <- pred$posterior - pred.flip$posterior
summary (diff)
boxplot (diff)
## mirror x axis of plslda scores
model <- plslda (X = iris [,1:4], grouping = iris[,5], ncomp=2)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.flip <- flip (model, 1)
pred.flip <- predict (model.flip)
points (pred.flip$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and flipped model's predictions
diff <- pred$posterior - pred.flip$posterior
summary (diff)
boxplot (diff)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[MASS]{lda}}

  \code{\link[cbmodels]{pcalda}}

  \code{\link[cbmodels]{plslda}}
}

