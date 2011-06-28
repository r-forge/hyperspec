##' command line completion for $
##' 
##' @aliases .DollarNames.hyperSpec
##' @author C. Beleites
##' @seealso \code{\link[utils]{.DollarNames}}
##' @export 
##' @callGraph
##' @keywords utilities
##' @log C. Beleites 2010-05-30: 
##' @title command line completion for $
##' @param x the hyperSpecobject
##' @param pattern pattern to look for
##' @return the name of the extra data slot


##’ command line completion for $
##’ command line completion for $
##’ 
##’ 
##’ @aliases .DollarNames.hyperSpec .DollarNames.hyperSpec
##’ @param x the hyperSpecobject
##’ @param pattern pattern to look for
##’ @return the name of the extra data slot
##’ @author C. Beleites
##’ @seealso \code{\link[utils]{.DollarNames}}
##’ @keywords utilities
.DollarNames.hyperSpec <- function (x, pattern)
  grep (pattern, colnames (x@data), value = TRUE)
