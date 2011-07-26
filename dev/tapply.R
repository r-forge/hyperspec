.tapply <- function (X, INDEX, FUN = NULL, ..., levelorder = TRUE){
  
  if (length (X$spc) == 0)
    stop ("empty spectra matrix.")
  
  FUN <- if (!is.null(FUN)) 
      match.fun(FUN)
  if (!is.list(INDEX)) 
      INDEX <- list(INDEX)
  
  
  
  spc <- tapply (X [, "spc", drop = FALSE], INDEX, FUN, ...)
  
  X
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title tapply for hyperSpec objects
##' @param X hyperSpec object
##' @param INDEX factor giving the groups
##' @param FUN aggregation function that works on a matrix
##' @param ... handed to \code{FUN}
##' @param simplify (doesn't apply to hyperSpec objects)
##' @param short,long,user,date handed to \code{\link{logentry}}
##' @param levelorder if TRUE, the rows of the result are in the order of the \code{levels
##' (INDEX)}. Otherwise,
##' @param drop should unused levels of \code{INDEX} be dropped?
##' @return hyperSpec object
##' @author Claudia Beleites
setMethod ("tapply", signature = signature (X = "hyperSpec", INDEX = "factor"),
           function (X, INDEX, FUN = NULL, ...,
                     simplify = stop ("not supported by hyperSpec objects"),
                     levelorder = TRUE, drop = TRUE, 
                     short = "tapply", long = NULL, user = NULL, date = NULL){
             validObject (X)

             if (drop) INDEX <- droplevels (INDEX)

             X <- .tapply (X@data, INDEX, FUN = FUN, ...)

             ## FUN could have changed the no. of columns of the spectra matrix
             validObject (X)

             .logentry(X, short = short, long = long, user = user, date = date)
           })
