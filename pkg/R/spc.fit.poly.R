###-----------------------------------------------------------------------------
###
###  spc.fit.poly
###
###
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

                                        

