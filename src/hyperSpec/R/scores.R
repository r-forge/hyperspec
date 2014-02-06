##' @noRd
setGeneric ("scores", function (object, ...) standardGeneric ("scores"))

##' @export
##' @rdname decomposition
##' @method scores hyperSpec
##' @S3method scores hyperSpec
##' @include decomposition.R
##' @include loadings.R
##' @import pls
##' @examples
##'
##' pca.scores <- scores (flu, pca$x)
##' plotc (pca.scores, groups = .wavelength)
##' 
scores.hyperSpec <- function (object, x, wavelength = seq_len (ncol (x)),
                              label.wavelength, label.spc, ...) {
  validObject (object)

  if (is.vector (x))
      dim (x) <- c(length (x), 1)

  if (nrow (x) != nrow (object))
      stop ("x and object must have the same number of rows.")

  attr (x, "class") <- "AsIs"
  object@data$spc <- x
  attr (object@data$spc, "class") <- NULL
  
  .wl (object) <- wavelength

  if (!missing (label.wavelength)) object@label$.wavelength <- label.wavelength

  rownames (object@data) <- rownames (x)
  
  if (!missing (label.spc)) object@label$spc <- label.spc
  
  validObject (object)

  object
}


.test (scores.hyperSpec) <- function (){
  rm (flu)
  rownames (flu) <- rownames (flu) # turns numeric into character
  
  pca <- prcomp (~ spc, data = flu)

  pca.scores <- scores (flu, pca$x [, 1 : 2])
  checkEquals (pca.scores$.., flu$.., check.attributes = FALSE)
  checkEqualsNumeric (wl (pca.scores), 1 : 2)
  checkEqualsNumeric (pca.scores [[]], pca$x [, 1 : 2])
  
  ## POSIXct
  flu$ct <- as.POSIXct(Sys.time()) 
  checkEquals (scores (flu, flu [[]])$ct, flu$ct)

  ## POSIXlt
  flu$lt <- as.POSIXlt(Sys.time()) 
  checkEquals (scores (flu, flu [[]])$lt, flu$lt)

  rm (flu)
}
