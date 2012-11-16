##' Flipping of models
##'
##' \code{flip} is a post-processing method for models. It flips (mirrors) dimensions of the model
##' space.
##' @title flip
##' @param object a model to flip
##' @param dims dimensions which should be mirrored 
##' @param ... further parameters
##' @return the flipped/mirrored model
##' @author Claudia Beleites
##' @export
flip <- function (object, dims, ...) UseMethod ("flip")

##' @noRd
flip.default <- function (object, ...){
  stop ("flip is not implemented for class '", class (object), "'.")
}
.test (flip.default) <- function (){
  checkException (flip ()) ## not defined
}

