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
##' @include hyperspec-package.R
vanderMonde <- function (x, order, ...){
  if (nargs () > 2)
    stop ('Unknown arguments: ', names (c (...)))
    
  outer (x, 0 : order, `^`)
}

##' @noRd
setGeneric ("vanderMonde")

##' @param x hyperSpec object
##' @param \dots hyperSpec method: further arguments to \code{\link{decomposition}}
##' @param normalize.wl function to transorm the wavelengths before evaluating the polynomial (or
##' other function). Use \code{\link[hyperSpec{normalize01}} to map the wavelength range to the interval [0, 1].
##' @return hyperSpec method: hyperSpec object containing van der Monde matrix as spectra and an additional column ".vdm.order" giving the order of each spectrum (term).
##' @rdname vanderMonde
##' @seealso \code{\link[hyperSpec]{wl.eval}} for calculating arbitrary functions of the wavelength,
##'
##' \code{\link[hyperSpec]{normalize01}} to normalize the wavnumbers before evaluating the function
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


.test (vanderMonde) <- function (){
  checkEqualsNumeric (vanderMonde (c (1 : 3, 5), 2),
                      matrix (c (1, 1, 1, 1, 1, 2, 3, 5, 1, 4, 9, 25), nrow = 4)
                      )
  checkException (vanderMonde (1, 0, normalize.wl = normalize01))

  checkTrue (chk.hy (vanderMonde (flu, 0)))
  checkTrue (validObject (vanderMonde (flu, 0)))
  
  checkEqualsNumeric (vanderMonde (paracetamol, 3)[[]],
                      t (vanderMonde (wl (paracetamol), 3)))

  checkEqualsNumeric (vanderMonde (paracetamol, 3, normalize.wl = normalize01)[[]],
                      t (vanderMonde (normalize01 (wl (paracetamol)), 3)))
}

##' @param x hyperSpec object
##' @param \dots hyperSpec method: expressions to be evaluated
##' @param normalize.wl function to transorm the wavelengths before evaluating the polynomial (or
##' other function). Use \code{\link[hyperSpec{normalize01}} to map the wavelength range to the interval [0, 1].
##' @return hyperSpec object containing one spectrum for each expression 
##' @export
##' @seealso \code{\link[hyperSpec]{vanderMonde}} for  polynomials,
##'
##' \code{\link[hyperSpec]{normalize01}} to normalize the wavnumbers before evaluating the function
##' @author C. Beleites
##' @examples
##' plot (wl.eval (laser, exp = function (x) exp (-x)))
wl.eval <- function (x, ..., normalize.wl = I){
  chk.hy (x)
  validObject (x)

  fun <- list (...)

  wl <- normalize.wl (x@wavelength)
  
  x <- decomposition (x, t (sapply (fun, function (f) f (wl))), scores = FALSE)
  x$.f <- if (is.null (names (fun)))
              rep (NA, length (fun))
          else
              names (fun)
  x
}

.test (wl.eval) <- function (){
  x <- runif (10, min = -1e3, max = 1e3)
  
  checkEqualsNumeric (min (normalize01 (x)), 0)
  checkEqualsNumeric (max (normalize01 (x)), 1)

  checkEqualsNumeric (normalize01 (x), (x - min (x)) / diff (range (x)))
}



##' @param x  vector with values to transform
##' @return vector with \code{x} values mapped to the interval [0, 1]: (x - min (x)) / diff (range (x))
##' @author C. Beleites
##' @seealso \code{\link[hyperSpec]{wl.eval}}, \code{\link[hyperSpec]{vanderMonde}}
##' @export 
normalize01 <- function (x){
  x <- x - min (x)
  x / max (x)
}

.test (normalize01) <- function (){
  x <- runif (10, min = -1e3, max = 1e3)
  
  checkEqualsNumeric (min (normalize01 (x)), 0)
  checkEqualsNumeric (max (normalize01 (x)), 1)

  checkEqualsNumeric (normalize01 (x), (x - min (x)) / diff (range (x)))
}
