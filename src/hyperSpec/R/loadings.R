## loadings needs special care: its interface is redefined by package pls.
## => Depend on pls, so interface is clear
## => still trouble, so do not provide a loadings method for now
## use function latentspc instead.

##' @noRd
setGeneric ("latentspc", function (object, ...) standardGeneric ("latentspc"))

.loadings <- function (object, x, label.spc, retain.columns = FALSE, ...){
  validObject (object)

  if (is.vector (x))
      dim (x) <- c(1, length (x))

  if (ncol (x) != nwl (object)){
      ## shortcut: many methods will return the latentvars-like results in columns
      if (nrow (x) == nwl (object)){
        x <- t (x)
        if (hy.getOption ("debuglevel") >= 1L)
            message ("Using t (x) instead of x: nrow (x) == nwl (object) !=  ncol (x).")
      } else {
        stop ("x must have the same number of columns as object has wavelengths.")
      }
    }
      
  spc <- match ("spc", colnames(object@data))
    
  ## `apply` changes type of retained columns to character!!!
  ## must be done in a loop one column after the other or a matrix in a column
  ## (e.g. for the independent variate of PLS) will cause an error 

  cols <- rep (TRUE, ncol (object@data))

  for (i in seq_len (ncol (object@data)) [-spc]) {
    tmp <- as.data.frame (lapply (object@data[, i, drop = FALSE], .na.if.different))
    object@data [1, i] <- tmp
    if (all (is.na (tmp)))
        cols [i] <- FALSE
  }

  if (!retain.columns) 
      object@data <- object@data[, cols, drop = FALSE]

  object@data <- object@data[rep(1, nrow(x)), , drop = FALSE]
  attr (x, "class") <- "AsIs"
  object@data$spc <- x
  attr (object@data$spc, "class") <- NULL

  rownames (object@data) <- rownames (x)
  
  if (!missing (label.spc)) object@label$spc <- label.spc
  
  validObject (object)

  object
}

.test (.loadings) <- function (){
  rm (flu)

  pca <- prcomp (~ spc, data = flu)
  
  ## retain.columns == FALSE
  pca.loadings <- latentspc (flu, t (pca$rotation [, 1 : 2]))
  checkEquals (ncol (pca.loadings$..), 0) 
  checkEqualsNumeric (pca.loadings [[]], t (pca$rotation [, 1 : 2]))

  ## retain.colums == TRUE
  pca.loadings <- latentspc (flu, t (pca$rotation [, 1 : 2]), retain.columns = TRUE)
  checkEquals (ncol (pca.loadings$..), ncol (flu$..))
  checkTrue (all (is.na (pca.loadings$..)))
  checkEqualsNumeric (pca.loadings [[]], t (pca$rotation [, 1 : 2]))

  ## latentspc should transpose as needed
  pca.loadings <- latentspc (flu, pca$rotation [, 1 : 2])
  checkEquals (ncol (pca.loadings$..), 0) 
  checkEqualsNumeric (pca.loadings [[]], t (pca$rotation [, 1 : 2]))

  ## single vector
  pca.loadings <- latentspc (flu, pca$center)
  checkEquals (ncol (pca.loadings$..), 0) 
  checkEqualsNumeric (pca.loadings [[]], pca$center)
  
  ## POSIXct
  flu$ct <- as.POSIXct(Sys.time()) 
  checkEquals (latentspc (flu, flu [[]])$ct, flu$ct)

  ## POSIXlt
  flu$lt <- as.POSIXlt(Sys.time()) 
  checkEquals (latentspc (flu, flu [[]])$lt, flu$lt)

  rm (flu)
}

##' @export
##' @rdname decomposition
##' @method latentspc hyperSpec
##' @docType methods
##' @include decomposition.R
##' @examples
##' pca <- prcomp (flu)
##' 
##' pca.loadings <- latentspc (flu, t (pca$rotation))
##' pca.center <- latentspc (flu, pca$center)
##'
##' plot (pca.center)
##' plot (pca.loadings, col = c ("red", "gray50"))
setMethod ("latentspc", signature = signature (object = "hyperSpec", x = "matrix"),  .loadings)

##' @noRd
setMethod ("latentspc", signature = signature (object = "hyperSpec", x = "numeric"), .loadings)
