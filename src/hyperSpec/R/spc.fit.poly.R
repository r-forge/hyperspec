###-----------------------------------------------------------------------------
###
###  spc.fit.poly
###
###


##’ Polynomial Baseline Fitting
##’ These functions fit polynomal baselines.
##’ 
##’ Both functions fit polynomials to be used as baselines. If \code{apply.to}
##’ is \code{NULL}, a \code{hyperSpec} object with the polynomial coefficients
##’ is returned, otherwise the polynomials are evaluated on the spectral range
##’ of \code{apply.to}.
##’ 
##’ \code{spc.fit.poly} calculates the least squares fit of order
##’ \code{poly.order} to the \emph{complete} spectra given in \code{fit.to}.
##’ Thus \code{fit.to} needs to be cut appropriately.
##’ 
##’ \code{spc.fit.poly.below} tries to fit the baseline on appropriate spectral
##’ ranges of the spectra in \code{fit.to}.  For details, see the
##’ \code{vignette ("baseline")}.
##’ 
##’ @aliases spc.fit.poly spc.fit.poly.below
##’ @param fit.to \code{hyperSpec} object on which the baselines are fitted
##’ @param apply.to \code{hyperSpec} object on which the baselines are evaluted
##’   If \code{NULL}, a \code{hyperSpec} object containing the polynomial
##’   coefficients rather than evaluted baselines is returned.
##’ @param poly.order order of the polynomial to be used
##’ @param npts.min minmal number of points used for fitting the polynomial
##’ @param noise noise level to be considered during the fit. It may be given
##’   as one value for all the spectra, or for each spectrum separately.
##’ @param short,user,date handed to \code{logentry}
##’ @return \code{hyperspec} object containing the baselines in the spectra
##’   matrix, either as polynomial coefficients or as polynomials evaluted on
##’   the spectral range of \code{apply.to}
##’ @author C. Beleites
##’ @seealso \url{../doc/baseline.pdf}
##’ @keywords manip datagen
##’ @examples
##’ 
##’ baselines <- spc.fit.poly(chondro[,, c (625 ~ 640, 1785 ~ 1800)], chondro)
##’ plot(chondro - baselines, "spcprctl5")
##’ 
##’ baselines <- spc.fit.poly.below(chondro)
##’ plot(chondro - baselines, "spcprctl5")
##’ 
##’ \dontrun{vignette ("baseline", package = "hyperSpec")}
##’ 
spc.fit.poly <- function (fit.to, apply.to = NULL, poly.order = 1,
                          short = "spc.fit.poly", user = NULL, date = NULL){
  chk.hy (fit.to)
  if (! is.null (apply.to))
    chk.hy (apply.to)

  validObject (fit.to)
  validObject (apply.to)

  x <- fit.to@wavelength
  x <- outer(x, 0 : poly.order, "^")             # Vandermonde matrix of x
  if (is.null (short)) short <- "spc.fit.poly: coefficients" 
  p <- apply (fit.to, 1, function (y, x){qr.solve (x, y)}, x,
              short = short, user = user, date = date)

  if (is.null (apply.to)){
    colnames (p@data$spc) <- paste ("x^", 0 : poly.order, sep="")

    p
  } else {
    wl <- apply.to@wavelength;
    x <- outer(wl, 0 : poly.order, "^")             # Vandermonde matrix of x
    apply.to@data$spc <- I (t (apply (p[[]], 1, function (p, x) {x %*% p}, x)))
    if (is.null (short)) short <- "spc.fit.poly: spectra" 
    apply.to <- .logentry (apply.to, short = short,
                           long = list (apply = match.call()$apply, poly.order = poly.order),
                           user = user, date = date)


    .wl(apply.to) <- wl
    colnames (apply.to@data$spc) <- format (wl, digits = 4)

    apply.to
  }
}

                                        

