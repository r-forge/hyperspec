##' PCA (\code{\link[stats]{prcomp}}) models are invariant to flipping of both a loading and its
##' score.
##' @seealso 
##' \code{\link[stats]{prcomp}}
##' @export
##' @include flip.R
##' @method flip prcomp
##' @S3method flip prcomp
##' @rdname flip
##' @examples
##'
##' ## flip 1st dimension (loading & score)
##' model <- prcomp (iris [-5])
##' model.flip <- flip (model, 1)
##' 
##' pairs (model$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' pairs (model.flip$x, col = iris$Species, pch = 19, cex = 0.3, asp = 1)
##' 
##' matplot (model$rotation, type = "l")
##' abline (h = 0)
##' matpoints (model.flip$rotation, type = "b")
##'
##' ## check difference between original model's predictions and rotated model's predictions
##' diff <- tcrossprod (model$x, model$rotation) - tcrossprod (model.flip$x, model.flip$rotation)
##' summary (diff)
##' boxplot (diff)
flip.prcomp <- function (object, dims = FALSE, ...){
  if (nargs () > 2)
      warning ("argument(s) ", paste (names (list (...)), collapse = ", "), " are ignored.")
  
  object$x [, dims] <- -object$x [, dims]
  object$rotation [, dims] <- -object$rotation [, dims]
  
  object
}

.test (flip.prcomp) <- function () {
  model <- prcomp (iris [-5], center = FALSE)
  x <- tcrossprod (model$x, model$rotation)

  for (d in list (1, 2, 2 : 3, -2, TRUE, FALSE)){
    model.flip <- flip (model, d)
    checkEqualsNumeric (tcrossprod (model.flip$x, model.flip$rotation), x)
  }
  
}

