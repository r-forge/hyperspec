##' Plot spectra matrix
##'
##' plots the spectra matrix
##' @param object hyperSpec object
##' @param y character giving the name of the extra data column to label the y axis.
##' @param ... further parameters for \code{\link[graphics]{image}}
##' @param contour should \code{\link[graphics]{contour}} be called instead of \code{\link[graphics]{image}}?
##' @author Claudia Beleites
##' @seealso  \code{\link[graphics]{image}}, \code{\link[graphics]{contour}}
##' @export 
##' @examples
##' plotmat (laser, col = alois.palette (100))
##' 
##' plot (laser, "mat")
##'
##' plotmat (laser)
##' plotmat (laser, contour = TRUE, add = TRUE)
##'
##' ## use different y axis labels
##'
##' plotmat (laser, "t")
##' 
##' plotmat (laser, laser$t / 3600, ylab = "t / h")
plotmat <- function (object, y = ".row", ylab, ..., contour = FALSE){

  chk.hy (object)
  validObject (object)

  if (is.character (y)) {
    if (missing (ylab))
      ylab <- switch (y,
                      .row = "row",
                      labels (object, y))
        
    y <- switch (y,
                 .row = seq_len (nrow (object)),
                 object@data [[y]])
  }
  
  dots <- modifyList (list (x = wl (object),
                            y = y,
                            z = t (object [[]]),
                            xlab = labels (object, ".wavelength"),
                            ylab = ylab
                            ),
                      list (...))
  
  if (contour)
    do.call ("contour", dots)
  else
    do.call ("image", dots)
}

