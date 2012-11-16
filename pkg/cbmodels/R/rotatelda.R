##' \code{\link[MASS]{lda}} models are invariant to rotation, i.e. the predicted posterior
##' probabilities and classes do not change if the LD scores \code{\link[MASS]{lda}$x} are rotated.
##' @seealso 
##' \code{\link[MASS]{lda}} 
##' @export
##' @include rotate.R
##' @method rotate lda
##' @S3method rotate lda
##' @rdname rotate
##' @examples
##'
##' ## clockwise rotation of lda
##' model <- lda (Species ~ ., data = iris)
##' pred <- predict (model)
##' plot (pred$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' 
##' model.rot <- rotate (model, rotmat (alpha = pi/40, ndim = ncol (model$scaling), 1, 2))
##' pred.rot <- predict (model.rot)
##' points (pred.rot$x, col = iris$Species, cex = 0.5)
##'
##' ## check difference between original model's predictions and rotated model's predictions
##' diff <- pred$posterior - pred.rot$posterior
##' summary (diff)
##' boxplot (diff)
rotate.lda <- function (object, R = NULL, ...){
  if (nargs () > 2)
      warning ("argument(s) ", paste (names (list (...)), collapse = ", "), " are ignored.")
  
  ndim <- ncol (object$scaling)
  
  ## check dimensions of R
  if (length (dim (R)) != 2 || any (dim (R) != ndim))
      stop ("R must be a square matrix of size (", ndim, " x ", ndim, ").")
  ## check R R' = I
  if (any (R %*% t (R) - diag (ndim) > .Machine$double.eps))
      stop ("R %*% t (R) != diag (", ndim, ")")

  object$scaling <- object$scaling %*% R

  object$svd [TRUE] <- NA
  
  object
}

.svdfix <- function (){
  ## guess:
  object <- lda (Species ~ ., data = sample (iris, 145), prior = c(3,1,1)/5)
  object <- lda (Species ~ ., data = iris)
  apply (predict (object, as.data.frame (object$means))$x, 2, sd)
  object$svd^2 / apply (predict (object, as.data.frame (object$means))$x, 2, sd)^2

  
  object$N*object$prior 

  object$counts
  ob

  pred <- predict (object, iris)$x
  p <- predict (object, as.data.frame (object$means))$x
 
  xin <- pred - p [as.numeric (iris$Species), ]

  p <- predict (object, iris)$x


  

  ## Was ist mit prior?

}

.test (rotate.lda) <- function () {
  model <- lda (Species ~ ., data = iris)
  pred <- predict (model)
  ## plot (pred$x, col = iris$Species, pch = 19, cex = 0.5, asp = 1)

  model.rot <- rotate (model, rotmat (alpha = 1, ndim = ncol (model$scaling), 1, 2))
  pred.rot <- predict (model.rot)
  ## points (pred.rot$x, col = iris$Species, cex = 0.5)
  
  checkEqualsNumeric (pred.rot$posterior, pred$posterior)
}

