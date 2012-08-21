##' Principal component analysis with reference spectra
##'
##' A PCA model is calculated after a multiple of the reference matrix is added to the data matrix.
##' 
##' @param x data matrix
##' @param reference reference data points
##' @param ... further arguments are handed to prcomp, but \code{center} is always \code{FALSE}.
##' @return object of class "pcaadd"
##' @rdname pcaadd
##' @include cbmodels.R
##' @export
setGeneric ("pcaadd", function (x, reference, ...){stop ("not supported")})
 

##' @rdname pcaadd
##' @param ref.factor reference is multiplied by \code{ref.factor * max (abs (range (x))) / max (abs
##' (range (reference))) * nrow (x) / nrow (reference)} before the PCA. This forces the first
##' directions towards \code{reference}.
##' @param refcomps the principal components that are attributed to the reference
##' @export
setMethod ("pcaadd", signature = signature (x = "matrix", reference = "matrix"),
           function (x, reference, ..., ref.factor = 10, refcomps = seq_len (nrow (reference))){

  ref.factor <- max (abs (range (x))) / max (abs (range (reference))) *
    nrow (x) / nrow (reference) * ref.factor

  x <- rbind (x, ref.factor * reference)

  pca <- prcomp (x, center = FALSE, ...)


  structure (list (pca = pca,
                   refcomps = refcomps),
             class = "pcaadd")
})

##' @rdname pcaadd
##' @export
setMethod ("pcaadd", signature = signature (x = "hyperSpec", reference = "hyperSpec"),
           function (x, reference, ...){
             validObject (x)
             validObject (reference)
             pcaadd (x[[]], reference [[]], ...)
           }
           )


##' @rdname pcaadd
##' @export
setMethod ("pcaadd", signature = signature (x = "hyperSpec", reference = "matrix"),
           function (x, reference, ...){
             validObject (x)
             pcaadd (x[[]], reference, ...)
           }
           )

##' @rdname pcaadd
##' @export
setMethod ("pcaadd", signature = signature (x = "matrix", reference = "hyperSpec"),
           function (x, reference, ...){
             validObject (reference)
             pcaadd (x, reference [[]], ...)
           }
           )

##' @rdname pcaadd
##' @param object pcaadd model
##' @param newdata matrix with new observations.
##' @method predict pcaadd
##' @S3method predict pcaadd
##' @export
predict.pcaadd <- function (object, newdata, ...){
  if (is (newdata, "hyperSpec")){
    chk.hy (newdata)
    validObject (newdata)
    newdata <- newdata [[]]
  }

  tmp <- predict (object$pca, newdata)
  tcrossprod (tmp [, - object$refcomps], object$pca$rotation [, -object$refcomps])
}

