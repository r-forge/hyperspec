##' Principal component analysis with reference spectra
##'
##' A PCA model is calculated after a multiple of the reference matrix is added to the data matrix.
##' 
##' @param x data matrix
##' @param reference reference data points
##' @param ref.factor reference is multiplied by \code{ref.factor * max (abs (range (x))) / max (abs
##' (range (reference))) * nrow (x) / nrow (reference)} before the PCA. This ensures that the first
##' directions are the ones given by \code{reference}.
##' @param \dots further arguments are handed to prcomp, but \code{center} is always \code{FALSE}.
##' @param refcomps the principal components that are attributed to the reference
##' @return object of class "pcaadd"
##' @export
pcaadd <- function (x, reference, ref.factor = 10, refcomps = seq_len (nrow (reference)), ...){

  ref.factor <- max (abs (range (x))) / max (abs (range (reference))) *
    nrow (x) / nrow (reference) * ref.factor

  x <- rbind (x, ref.factor * reference)

  pca <- prcomp (x, center = FALSE, ...)


  structure (list (pca = pca,
                   refcomps = refcomps),
             class = "pcaadd")
}

##' @rdname pcaadd
##' @param \dots the hyperSpec-method hands further arguments to the generic.
##' @export
setMethod ("pcaadd", signature = signature (x = "hyperSpec", reference = "hyperSpec"),
           function (x, reference, ...){
             validObject (x)
             validObject (reference)
             pcaadd (x[[]], reference [[]], ...)
           }
           )


##' @rdname pcaadd
##' @param object pcaadd model
##' @param newdata matrix with new observations.
##' @export
predict.pcaadd <- function (object, newdata){
  tmp <- predict (object$pca, newdata)
  tcrossprod (dummy [, - object$refcomps], object$pca$rotation [, -object$refcomps])
}
