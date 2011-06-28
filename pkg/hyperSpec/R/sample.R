##’ Random Samples and Permutations
##’ Take a sample of the specified size from the elements of x with or without
##’ replacement.
##’ 
##’ \code{isample} returns an vector of indices, \code{sample} returns the
##’ corresponding hyperSpec object.
##’ 
##’ @name sample
##’ @aliases sample-methods sample isample sample,hyperSpec-method
##’ @docType methods
##’ @param x The hyperSpec object to sample from
##’ @param size positive integer giving the number of spectra to choose.
##’ @param replace Should sampling be with replacement?
##’ @param prob A vector of probability weights for obtaining the elements of
##’   the vector being sampled.
##’ @param short,user,date are handed to \code{\link{logentry}}
##’ @return a hyperSpec object for \code{sample}, and an integer vector for
##’   \code{isample} that is suitable for indexing (into the spectra) of x.
##’ @author C. Beleites
##’ @seealso \code{\link[base]{sample}}
##’ @keywords methods distribution
##’ @examples
##’ 
##’ sample (flu, 3)
##’ isample (flu, 3)
##’ isample (flu, 3, replace = TRUE)
##’ isample (flu, 8, replace = TRUE)
##’ 
##’ plot (flu, col = "darkgray")
##’ plot (sample (flu, 3), col = "red", add = TRUE)
##’ 
##’ plot (flu, col = "darkgray")
##’ plot (sample (flu, 3, replace = TRUE), col = "#0000FF80", add = TRUE,
##’       lines.args = list (lwd = 2));
##’ 
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


