##' \code{\link[MASS]{lda}} models are invariant to flipping, i.e. the predicted posterior
##' probabilities and classes do not change if the LD scores \code{\link[MASS]{lda}$x} are multiplied
##' by -1.
##' @seealso 
##' \code{\link[MASS]{lda}} 
##' @export
##' @include flip.R
##' @method flip lda
##' @S3method flip lda
##' @rdname flip
##' @examples
##'
##' ## flip 1st dimension
##' model <- lda (Species ~ ., data = iris)
##' pred <- predict (model)
##' plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' 
##' model.flip <- flip (model, 1)
##' pred.flip <- predict (model.flip)
##' points (pred.flip$x, col = iris$Species, cex = 0.5)
##'
##' ## check difference between original model's predictions and rotated model's predictions
##' diff <- pred$posterior - pred.flip$posterior
##' summary (diff)
##' boxplot (diff)
flip.lda <- function (object, dims = FALSE, ...){
  if (nargs () > 2)
      warning ("argument(s) ", paste (names (list (...)), collapse = ", "), " are ignored.")
  
  object$scaling [, dims] <- -object$scaling [, dims] 
  
  object
}

.test (flip.lda) <- function () {
  model <- lda (Species ~ ., data = iris)
  pred <- predict (model)

  for (d in list (1, 2, 1 : 2, -2, TRUE, FALSE)){
    model.flip <- flip (model, d)
    pred.flip <- predict (model.flip)
    checkEqualsNumeric (pred.flip$posterior, pred$posterior)
  }
  
}

