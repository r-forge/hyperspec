##' Function evaluation on hyperSpec objects
##'
##' vandermonde generates van der Monde matrices for the wavelengths of a hyperSpec object.
##' @param x vector with values to evaluate the polynomial on
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
##' @param normalize.wl function to transorm the wavelengths before evaluating the polynomial (or
##' other function). Use \code{normalize01} to map the wavelength range to the interval [0, 1].
##' @return hyperSpec method: hyperSpec object containing van der Monde matrix as spectra and an additional column ".vdm.order" giving the order of each spectrum (term).
##' @rdname vanderMonde
##' @export
##' @examples
##' plot (vanderMonde (flu, 2))
##' plot (vanderMonde (flu, 2, normalize.wl = normalize01))
setMethod ("vanderMonde", signature = signature (x = "hyperSpec"),
           function (x, order, ..., normalize.wl = I){
  validObject (x)

  wl <- normalize.wl (x@wavelength)
  
  x <- decomposition (x, t (vanderMonde (wl, order)), scores = FALSE, ...)
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
wl.eval <- function (x, ..., normalize.wl = I){
  chk.hy (x)
  validObject (x)

  fun <- list (...)

  wl <- normalize.wl (x@wavelength)
  
  x <- decomposition (x, t (sapply (fun, function (f) f (wl))), scores = FALSE, ...)
  x$.f <- if (is.null (names (fun)))
              rep (NA, length (fun))
          else
              names (fun)
  x
}

##' @param x  vector with values to transform
##' @return vector with \code{x} values mapped to the interval [0, 1]: (x - min (x)) / diff (range (x))
##' @rdname vanderMonde
##' @export 
normalize01 <- function (x){
  x <- x - min (x)
  x / max (x)
}
