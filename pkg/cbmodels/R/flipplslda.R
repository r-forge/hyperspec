##' @seealso 
##' \code{\link[cbmodels]{plslda}} 
##' @export
##' @include flip.R
##' @include fliplda.R
##' @rdname flip
##' @method flip plslda
##' @S3method flip plslda
##' @examples
##'
##' ## mirror x axis of plslda scores
##' model <- plslda (X = iris [,1:4], grouping = iris[,5], ncomp=2)
##' pred <- predict (model)
##' plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' 
##' model.flip <- flip (model, 1)
##' pred.flip <- predict (model.flip)
##' points (pred.flip$x, col = iris$Species, cex = 0.5)
##'
##' ## check difference between original model's predictions and flipped model's predictions
##' diff <- pred$posterior - pred.flip$posterior
##' summary (diff)
##' boxplot (diff)
flip.plslda <- function (object, dims = FALSE, ...){
  object$lda <- flip (object$lda, dims = dims, ...)
  
  object
}

.test (flip.plslda) <- function () {
  model <- plslda (X = iris [, -5], grouping = iris [,5], comps = 1:3)
  pred <- predict (model)

  for (d in list (1, 2, 1 : 2, TRUE, FALSE)){
    model.flip <- flip (model, d)
    pred.flip <- predict (model.flip)
    checkEqualsNumeric (pred.flip$posterior, pred$posterior)
  }
}

