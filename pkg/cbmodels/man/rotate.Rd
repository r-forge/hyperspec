\name{rotate}
\alias{rotate}
\alias{rotate.lda}
\alias{rotate.pcalda}
\alias{rotate.plslda}
\title{rotate}
\usage{
  rotate(object, R, ...)

  \method{rotate}{lda} (object, R = NULL, ...)

  \method{rotate}{pcalda} (object, R = NULL, ...)

  \method{rotate}{plslda} (object, R = NULL, ...)
}
\arguments{
  \item{object}{a model to rotate}

  \item{R}{rotation matrix, see
  \code{\link[cbmodels]{rotmat}} and example}

  \item{...}{further parameters}
}
\value{
  the rotated model
}
\description{
  Rotation of models

  \code{\link[MASS]{lda}} models are invariant to rotation,
  i.e. the predicted posterior probabilities and classes do
  not change if the LD scores \code{\link[MASS]{lda}$x} are
  rotated.
}
\details{
  Rotate is a post-processing method for models.
}
\examples{
## clockwise rotation of lda
model <- lda (Species ~ ., data = iris)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.rot <- rotate (model, rotmat (alpha = pi/40, ndim = ncol (model$scaling), 1, 2))
pred.rot <- predict (model.rot)
points (pred.rot$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and rotated model's predictions
diff <- pred$posterior - pred.rot$posterior
summary (diff)
boxplot (diff)
## clockwise rotation of pcalda
model <- pcalda (X = iris [,1:4], grouping = iris$Species, comps=1:3)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.rot <- rotate (model, rotmat (alpha = pi/40, ndim = 2, 1, 2))
pred.rot <- predict (model.rot)
points (pred.rot$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and rotated model's predictions
diff <- pred$posterior - pred.rot$posterior
summary (diff)
boxplot (diff)
## clockwise rotation of plslda
model <- plslda (X = iris [,1:4], grouping = iris[,5], ncomp=2)
pred <- predict (model)
plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)

model.rot <- rotate (model, rotmat (alpha = pi/40, ndim = 2, 1, 2))
pred.rot <- predict (model.rot)
points (pred.rot$x, col = iris$Species, cex = 0.5)

## check difference between original model's predictions and rotated model's predictions
diff <- pred$posterior - pred.rot$posterior
summary (diff)
boxplot (diff)
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link[cbmodels]{rotmat}} for calculating rotation
  matrices

  \code{\link[MASS]{lda}}

  \code{\link[cbmodels]{pcalda}}

  \code{\link[cbmodels]{plslda}}
}

