###-----------------------------------------------------------------------------
###
###  matlab.palette
###
###



##’ Matlab-like Palettes
##’ Two palettes going from blue over green to red, approximately as the
##’ standard palette of Matlab does. The second one has darker green values and
##’ is better suited for plotting lines on white background.
##’ 
##’ 
##’ @aliases matlab.palette matlab.dark.palette
##’ @param n the number of colors to be in the palette.
##’ @param \dots further arguments are handed to
##’   \code{\link[grDevices]{rainbow}}
##’ @return A vector containing the color values in the form "\#rrbbggaa".
##’ @author C. Beleites
##’ @seealso \code{\link[grDevices]{rainbow}}
##’ @keywords color
##’ @examples
##’ 
##’ plotmap (chondro [,, 778], col.regions = matlab.palette ())
##’ plot (flu, col = matlab.dark.palette (nrow (flu)))
##’ 
matlab.palette <- function (n = 100, ...) {
  rev (rainbow (n, start = 0, end = 4/6, ...))
}


