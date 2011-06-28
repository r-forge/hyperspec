##’ Arithmetical Operators: +, -, *, /, ^, %%, %/%, %*%
##’ The arithmetical operators \code{+}, \code{-}, \code{*}, \code{/},
##’ \code{\^}, \code{%%}, \code{%/%}, and \code{%*%} for \code{hyperSpec}
##’ objects.
##’ 
##’ You can use these operators in different ways: \preformatted{ e1 + e2 `+`
##’ (e1, e2)
##’ 
##’ x %*% y `%*%`(x, y)
##’ 
##’ -x } The arithmetical operators \code{+}, \code{-}, \code{*}, \code{/},
##’ \code{^}, \code{%%}, \code{%/%}, and \code{%*%} work on the spectra matrix
##’ of the \code{hyperSpec} object. They have their usual meaning (see
##’ \code{\link[base]{Arithmetic}}).  The operators work also with one
##’ \code{hyperSpec} object and a numeric object or a matrices of the same size
##’ as the spectra matrix of the \code{hyperSpec} object.
##’ 
##’ With numeric vectors \code{\link[hyperSpec]{sweep}} is most probably more
##’ appropriate.
##’ 
##’ If you want to calculate on the \code{data.frame} \code{hyperSpec@data},
##’ you have to do this directly on \code{hyperSpec@data}.
##’ 
##’ @name Arith
##’ @aliases hyperSpec Arith Arith-method Arith,hyperSpec-method
##’   Arith,hyperSpec,hyperSpec-method +,hyperSpec,hyperSpec-method
##’   -,hyperSpec,hyperSpec-method *,hyperSpec,hyperSpec-method
##’   ^,hyperSpec,hyperSpec-method %%,hyperSpec,hyperSpec-method
##’   %/%,hyperSpec,hyperSpec-method /,hyperSpec,hyperSpec-method
##’   Arith,hyperSpec,matrix-method Arith,hyperSpec,numeric-method
##’   Arith,hyperSpec,missing-method Arith,matrix,hyperSpec-method
##’   Arith,numeric,hyperSpec-method %*% %*%,hyperSpec,hyperSpec-method
##’   %*%,matrix,hyperSpec-method %*%,hyperSpec,matrix-method
##’ @docType methods
##’ @param e1,e2 Either two \code{hyperSpec} objects or one \code{hyperSpec}
##’   object and matrix of same size as \code{hyperSpec[[]]} or a scalar
##’   (numeric of length 1).
##’ @param x,y Either two \code{hyperSpec} objects or one \code{hyperSpec}
##’   object and one matrix of appropriate size.
##’ @return \code{hyperSpec} object with the new spectra matrix.
##’ @author C. Beleites
##’ @seealso \code{\link[hyperSpec]{sweep-methods}} for calculations involving
##’   a vector and the spectral matrix.
##’ 
##’ \code{\link[methods]{S4groupGeneric}} for group generic methods.
##’ 
##’ \code{\link[base]{Arithmetic}} for the base arithmetic functions.
##’ 
##’ \code{\link[base]{matmult}} for matrix multiplications with \code{%*%}.
##’ 
##’ \code{\link[hyperSpec]{Comparison}} for comparison operators,
##’   \code{\link[hyperSpec]{Math}} for mathematical group generic functions
##’   (Math and Math2 groups) working on \code{hyperSpec} objects.
##’ @keywords methods arith
##’ @examples
##’ 
##’ chondro + chondro
##’ 1 / chondro
##’ all((chondro + chondro - 2 * chondro)[[]] == 0)
##’ -flu
##’ 


setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             if (.Generic %in% c ("*", "^", "%%", "%/%", "/"))
               warning (paste ("Do you really want to use", .Generic, "on 2 hyperSpec objects?"))
             e1 [[]] <- callGeneric (e1[[]], e2[[]])
             .logentry (e1, short = .Generic, long = as.character (e2))
           }
           )

.arithx <- function (e1, e2){
  validObject (e1)
  if (missing (e2)){
    e1  [[]] <- callGeneric (e1 [[]])
    .logentry (e1, short = .Generic, long = list ())
  } else {
    e1  [[]] <- callGeneric (e1 [[]], e2)
    .logentry (e1, short = .Generic,
               long = list (e2 = .paste.row (e2, val = TRUE)))
  }
}

.arithy <- function (e1, e2){
  validObject (e2)
  e2  [[]] <- callGeneric (e1, e2 [[]])
  .logentry (e2, short = .Generic, long = list (e1 = .paste.row (e1, val = TRUE)))
}

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "numeric"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "matrix"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "missing"), .arithx)

setMethod ("Arith", signature (e1 = "numeric", e2 = "hyperSpec"), .arithy)
setMethod ("Arith", signature (e1 = "matrix", e2 = "hyperSpec"), .arithy)
