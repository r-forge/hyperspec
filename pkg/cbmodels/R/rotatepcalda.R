##' @seealso 
##' \code{\link[cbmodels]{pcalda}} 
##' @export
##' @include rotate.R
##' @include rotatelda.R
##' @method rotate pcalda
##' @S3method rotate pcalda
##' @rdname rotate
##' @examples
##'
##' ## clockwise rotation of pcalda
##' model <- pcalda (X = iris [,1:4], grouping = iris$Species, comps=1:3)
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
rotate.pcalda <- function (object, R = NULL, ...){
  object$lda <- rotate (object$lda, R = R, ...)
  
  object
}

.test (rotate.pcalda) <- function () {
  model <- pcalda (X = iris [, -5], grouping = iris [,5], comps = 1:3)
  pred <- predict (model)
  ## plot (pred$x, col = iris$Species, pch = 19, cex = 0.5, asp = 1)

  model.rot <- rotate (model, rotmat (alpha = 1, ndim = 2, 1, 2))
  pred.rot <- predict (model.rot)
  ## points (pred.rot$x, col = iris$Species, cex = 0.5)
  
  checkEqualsNumeric (pred.rot$posterior, pred$posterior)
}

