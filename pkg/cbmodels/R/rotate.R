##' Rotation of models
##'
##' Rotate is a post-processing method for models. 
##' @title rotate
##' @param object a model to rotate
##' @param R rotation matrix, see \code{\link[cbmodels]{rotmat}} and example
##' @param ... further parameters
##' @return the rotated model
##' @author Claudia Beleites
##' @seealso  \code{\link[cbmodels]{rotmat}} for calculating rotation matrices
##' @export
rotate <- function (object, R, ...) UseMethod ("rotate")

##' @noRd
rotate.default <- function (object, ...){
  stop ("rotate is not implemented for class '", class (object), "'.")
}
.test (rotate.default) <- function (){
  checkException (rotate (1)) ## not defined
}

