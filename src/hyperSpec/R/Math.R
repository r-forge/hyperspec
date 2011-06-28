##’ Math Functions for hyperSpec Objects
##’ The functions
##’ 
##’ \code{all}, \code{any},
##’ 
##’ \code{sum}, \code{prod},
##’ 
##’ \code{min}, \code{max}, and
##’ 
##’ \code{range}
##’ 
##’ for \code{hyperSpec} objects.
##’ 
##’ All these functions work on the spectra matrix.
##’ 
##’ @name Summary
##’ @aliases Summary,hyperSpec-method hyperSpec Summary all,hyperSpec-method
##’   any,hyperSpec-method sum,hyperSpec-method prod,hyperSpec-method
##’   min,hyperSpec-method max,hyperSpec-method range,hyperSpec-method
##’   is.na,hyperSpec-method
##’ @docType methods
##’ @param x the \code{hyperSpec} object
##’ @param \dots further objects
##’ @param na.rm logical indicating whether missing values should be removed
##’ @return a numeric, for \code{is.na} a logcical matrix
##’ @author C. Beleites
##’ @seealso \code{\link[methods]{S4groupGeneric}} for group generic methods.
##’ 
##’ \code{\link[base]{Summary}} for the base summary functions.
##’ 
##’ \code{\link[hyperSpec]{Arith}} for arithmetic operators,
##’   \code{\link[hyperSpec]{Comparison}} for comparison operators, and
##’   \code{\link[hyperSpec]{Math}} for group generic functions working on
##’   \code{hyperSpec} objects.
##’ @keywords methods arith
##’ @examples
##’ 
##’ 	range (flu) 
##’ 

setMethod ("Math", signature (x = "hyperSpec"),
           function (x){
             validObject (x)

             if (grepl ("^cum", .Generic) || grepl ("gamma$", .Generic))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))

             x [[]] <- callGeneric (x[[]])
             .logentry (x, short = .Generic, long = list())
           }
           )
