##' check whether an object is a hyperSpec object
##' 
##'  to be used like validObject
##' 
##' @aliases chk.hy
##' @author C. Beleites
##' @seealso \code{\link[methods]{validObject}}
##' @export 
##' @callGraph
##' @keywords methods
##' @examples 
##' chk.hy (chondro)
##' @param x the object to check

chk.hy <- function (x){
  if (! is (x, "hyperSpec"))
    stop ("no hyperSpec object")

  TRUE
}

