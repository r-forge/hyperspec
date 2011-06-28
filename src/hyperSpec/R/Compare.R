##’ Comparison Operators: <, >, <=, >=, ==, and !=
##’ The comparison operators \code{>}, \code{<}, \code{>=}, \code{<=},
##’ \code{==}, and \code{!=} for \code{hyperSpec} objects.
##’ 
##’ \code{all.equal} checks the equality of two hyperSpec objects.
##’ 
##’ The comparison operators \code{>}, \code{<}, \code{>=}, \code{<=},
##’ \code{==}, and \code{!=} work on the spectra matrix of the \code{hyperSpec}
##’ object. They have their usual meaning (see \code{\link[base]{Comparison}}).
##’ The operators work also with one \code{hyperSpec} object and a numeric
##’ (scalar) object or a matrices of the same size as the spectra matrix of the
##’ \code{hyperSpec} object.
##’ 
##’ With numeric vectors \code{\link[hyperSpec]{sweep}} might be more
##’ appropriate.
##’ 
##’ If you want to calculate on the \code{data.frame} \code{hyperSpec@data},
##’ you have to do this directly on \code{hyperSpec@data}.
##’ 
##’ @name Compare
##’ @aliases hyperSpec Comparison hyperSpec compare hyperSpec Comparison
##’   Operators Compare,hyperSpec-method Compare,hyperSpec,hyperSpec-method
##’   <,hyperSpec,hyperSpec-method >,hyperSpec,hyperSpec-method
##’   <=,hyperSpec,hyperSpec-method >=,hyperSpec,hyperSpec-method
##’   ==,hyperSpec,hyperSpec-method !=,hyperSpec,hyperSpec-method
##’   Compare,hyperSpec,matrix-method Compare,hyperSpec,numeric-method
##’   Compare,matrix,hyperSpec-method Compare,numeric,hyperSpec-method
##’   all.equal,hyperSpec,hyperSpec-method
##’ @docType methods
##’ @param e1,e2 Either two \code{hyperSpec} objects or one \code{hyperSpec}
##’   object and matrix of same size as \code{hyperSpec[[]]} or a scalar
##’   (numeric of length 1).
##’ @param target,current two \code{hyperSpec} objects that are tested for
##’   equality
##’ @param \dots handed to \code{all.equal} when testing the slots of the
##’   \code{hyperSpec} objects
##’ @param check.column.order If two objects have the same data, but the order
##’   of the columns (determined by the names) differs, should they be regarded
##’   as different?
##’ @param check.label Should the slot \code{label} be checked? \cr If the
##’   labels differ only in the order of their entries, they are conidered
##’   equal.
##’ @param check.log Should the slot \code{label} be checked?
##’ @return a logical matrix for the comparison operators.
##’ 
##’ As \code{hyperSpec} objects must have numeric spectra matrices, the
##’   resulting matrix of the comparison is returned directly.
##’ 
##’ \code{all.equal} returns either \code{TRUE}, or a character vector
##’   describing the differences. In conditions, the result must therefore be
##’   tested with \code{\link[base]{isTRUE}}.
##’ @author C. Beleites
##’ @seealso \code{\link[hyperSpec]{sweep-methods}} for calculations involving
##’   a vector and the spectral matrix.
##’ 
##’ \code{\link[methods]{S4groupGeneric}} for group generic methods.
##’ 
##’ \code{\link[base]{Comparison}} for the base comparison functions.
##’ 
##’ \code{\link[hyperSpec]{Arith}} for arithmetic operators,
##’   \code{\link[hyperSpec]{Math}} for mathematical group generic functions
##’   (groups Math and Math2) working on \code{hyperSpec} objects.
##’ 
##’ \code{\link[base]{all.equal}} and \code{\link[base]{isTRUE}}
##’ @keywords methods arith
##’ @examples
##’ 
##’ flu [,,445 ~ 450] > 300
##’ 
##’ all (flu == flu[[]])
##’ 

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)

             callGeneric (e1[[]], e2[[]])
           }
           )

.compx <- function (e1, e2){
  validObject (e1)
  callGeneric (e1 [[]], e2)
}

.compy <- function (e1, e2){
  validObject (e2)
  callGeneric (e1, e2 [[]])
}

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "numeric"), .compx)
setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "matrix"), .compx)

setMethod ("Compare", signature (e1 = "numeric", e2 = "hyperSpec"), .compy)
setMethod ("Compare", signature (e1 = "matrix", e2 = "hyperSpec"), .compy)


