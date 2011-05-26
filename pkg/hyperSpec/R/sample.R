###------------------------------------------------------------------------------
###
###  sample
###

setMethod ("sample", signature = "hyperSpec",
           function (x, size = nrow (x), replace = FALSE, prob = NULL,
                     short = "sample", user = NULL, date = NULL
                     ) {
             validObject (x)

             s <- sample.int (nrow (x@data), size = size, replace = replace, prob = prob)

             .logentry (x [s], short = short,
                        long = list (size = size, replace = replace, prob = prob,
                        user = user, date = date))
           }
           )

##' @rdname sample
##' @export
##' @author C. Beleites
##' @seealso \code{\link[base]{sample}}
##' @param x hyperSpec object
##' @param size number of spectra to draw
##' @param replace draw with replacement?
##' @param prob probablilities for each spectrum
##' @return vector with indices suitable for row-indexing x
##' @examples
##' isample (chondro, 3)
##' isample (chondro, 3, replace = FALSE)
isample <- function (x, size = nrow (x), replace = FALSE, prob = NULL) {
  chk.hy (x)
  validObject (x)

  sample.int (nrow (x), size = size, replace = replace, prob = prob)
}


