###------------------------------------------------------------------------------
###
###  sample
###

setMethod ("sample", signature = "hyperSpec",
           function (x, size, replace = FALSE, prob = NULL  
                     ## short = "sample", user = NULL, date = NULL
                     ## to be done in 2.11 - when sample is implicit Generic
                     ) {
             validObject (x)

             s <- sample.int (nrow (x@data), size = size, replace = replace, prob = prob)

             .logentry (x [s], short = "sample",
                        long = list (size = size, replace = replace, prob = prob))
## for 2.11                        user = user, date = date)
           }
           )

##' @rdname sample
##' @export
##' @author C. Beleites
##' @seealso sample
##' @param x
##' @param size
##' @param replace
##' @param prob
##' @return vector with indices suitable for row-indexing x
##' @examples
##' isample (chondro, 3)
##' isample (chondro, 3, replace = FALSE)
isample <- function (x, size, replace = FALSE, prob = NULL) {
  .is.hy (x)
  validObject (x)

  sample.int (nrow (x), size = size, replace = replace, prob = prob)
}


