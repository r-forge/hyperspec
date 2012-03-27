##' Function evaluation on hyperSpec objects
##'
##' vandermonde generates van der Monde matrices for the wavelengths of a hyperSpec object.
##' @param x
##' @param order of the polynomial
##' @param \dots ignored by the standard generic
##' @rdname vanderMonde
##' @return van der Monde matrix
##' @author C. Beleites
##' @export
vanderMonde <- function (x, order, ...){
  outer (x, 0 : order, `^`)
}

##' @noRd
setGeneric ("vanderMonde")

##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @return hyperSpec method: hyperSpec object containing van der Monde matrix as spectra and an additional column ".vdm.order" giving the order of each spectrum (term).
##' @rdname vanderMonde
##' @export
setMethod ("vanderMonde", signature = signature (x = "hyperSpec"), function (x, order, ...){
  validObject (x)
  
  x <- decomposition (x, t (vanderMonde (x@wavelength, order)), scores = FALSE, ...)
  x$.vdm.order <- 0 : order
  x
})



##' @param x hyperSpec object
##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @return hyperSpec object containing one spectrum for each expression 
##' @rdname vanderMonde
##' @export
##' @examples
##' plot (wl.eval (laser, exp = function (x) exp (-x)))
wl.eval <- function (x, ...){
  chk.hy (x)
  validObject (x)

  fun <- list (...)
 
  x <- decomposition (x, t (sapply (fun, function (f) f (x@wavelength))), scores = FALSE, ...)
  x$.f <- if (is.null (names (fun)))
              rep (NA, length (fun))
          else
              names (fun)
  x
}
