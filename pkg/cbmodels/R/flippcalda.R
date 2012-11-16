##' @seealso 
##' \code{\link[cbmodels]{pcalda}} 
##' @export
##' @include flip.R
##' @include fliplda.R
##' @method flip pcalda
##' @S3method flip pcalda
##' @rdname flip
##' @examples
##'
##' ## mirror x axis
##' model <- pcalda (X = iris [,1:4], grouping = iris$Species, comps=1:3)
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
flip.pcalda <- function (object, dims = FALSE, ...){
  object$lda <- flip (object$lda, dims = dims, ...)
  
  object
}

.test (flip.pcalda) <- function () {
  model <- pcalda (X = iris [, -5], grouping = iris [,5], comps = 1:3)
  pred <- predict (model)

  for (d in list (1, 2, 1 : 2, TRUE, FALSE)){
    model.flip <- flip (model, d)
    pred.flip <- predict (model.flip)
    checkEqualsNumeric (pred.flip$posterior, pred$posterior)
  }
}

