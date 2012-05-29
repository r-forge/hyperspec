##' center
##'
##' helper function returning the centering parameters of models.
##'
##' 
##' @export
##' @param object the model
##' @param ... further argument to the overloaded functions.
##' @author C. Beleites
##' @seealso \code{\link{pcalda}}, \code{\link{plslda}}
center <- function (object, ...)
  UseMethod ("center")
