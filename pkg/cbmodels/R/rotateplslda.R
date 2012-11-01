##' @seealso 
##' \code{\link[cbmodels]{plslda}} 
##' @export
##' @include rotate.R
##' @include rotatelda.R
##' @rdname rotate
##' @method rotate plslda
##' @S3method rotate plslda
##' @examples
##'
##' ## clockwise rotation of plslda
##' model <- plslda (X = iris [,1:4], grouping = iris[,5], ncomp=2)
##' pred <- predict (model)
##' plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' 
##' model.rot <- rotate (model, rotmat (alpha = pi/40, ndim = 2, 1, 2))
##' pred.rot <- predict (model.rot)
##' points (pred.rot$x, col = iris$Species, cex = 0.5)
##'
##' ## check difference between original model's predictions and rotated model's predictions
##' diff <- pred$posterior - pred.rot$posterior
##' summary (diff)
##' boxplot (diff)
rotate.plslda <- function (object, R = NULL, ...){
  object$lda <- rotate (object$lda, R = R, ...)
  
  object
}

.test (rotate.plslda) <- function () {
  model <- plslda (X = iris [, -5], grouping = iris [,5], comps = 1:3)
  pred <- predict (model)
  ## plot (pred$x, col = iris$Species, pch = 19, cex = 0.5, asp = 1)

  model.rot <- rotate (model, rotmat (alpha = 1, ndim = 2, 1, 2))
  pred.rot <- predict (model.rot)
  ## points (pred.rot$x, col = iris$Species, cex = 0.5)
  
  checkEqualsNumeric (pred.rot$posterior, pred$posterior)
}

