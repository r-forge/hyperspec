.tapply <- function (X, INDEX, FUN = NULL, ..., levelorder = TRUE){
  X
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param X hyperSpec object
##' @param INDEX 
##' @param FUN aggregation function that works on a matrix
##' @param ...
##' @param short,long,user,date handed to \code{\link{logentry}}
##' @param levelorder if TRUE, the rows of the result are in the order of the \code{levels
##' (INDEX)}. Otherwise,
##' @param drop should unused levels of \code{INDEX} be dropped?
##' @return 
##' @author Claudia Beleites
##' @seealso 
##' @export 
##' @callgraph 
setMethod ("tapply", signature = signature (X = "hyperSpec", INDEX = "factor"),
           function (X, INDEX, FUN = NULL, ..., levelorder = TRUE, drop = TRUE, 
                     short = "tapply", long = NULL, user = NULL, date = NULL){
             validObject (X)

             if (drop) INDEX <- droplevels (INDEX)

             X <- .tapply (X@data, INDEX, FUN = FUN, ...)

             ## FUN could have changed the no. of columns of the spectra matrix
             validObject (X)

             .logentry(X, short = short, long = long, user = user, date = date)
           })
