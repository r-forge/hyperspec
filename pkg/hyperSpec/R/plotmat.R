##' Plot spectra matrix
##'
##' plots the spectra matrix
##' @param x hyperSpec object
##' @param ... further parameters for \code{\link[graphics]{image}}
##' @param contour should \code{\link[graphics]{contour}} be called instead of \code{\link[graphics]{image}}?
##' @author Claudia Beleites
##' @seealso  \code{\link[graphics]{image}}, \code{\link[graphics]{contour}}
##' @export 
##' @examples
##' plotmat (laser, col = alois.palette (100))
##' plot (laser, "mat")
##' plotmat (laser)
##' plotmat (laser, contour = TRUE, add = TRUE)
plotmat <- function (x, ..., contour = FALSE){
  chk.hy (x)
  validObject (x)
  
  dots <- modifyList (list (x = wl (x),
                            y = seq_len (nrow (x)),
                            z = t (x [[]]),
                            xlab = labels (x, ".wavelength"),
                            ylab = "row"
                            ),
                      list (...))

  if (contour)
    do.call ("contour", dots)
  else
    do.call ("image", dots)
}

