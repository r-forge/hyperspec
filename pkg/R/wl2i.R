###-----------------------------------------------------------------------------
###
### wl2i
###
###
wl2i <- function (x, wavelength = stop ("wavelengths are required.")){
  .is.hy (x)
  validObject (x)

  ## special in formula
  max <- max (x@wavelength)
  min <- min (x@wavelength)

  `~` <- function (e1, e2){
    if (missing (e2))              # happens with formula ( ~ end)
      stop ("wavelength must be a both-sided formula")

    if (    (Re (e1) < min (x@wavelength) && Re (e2) < min (x@wavelength)) ||
        (Re (e1) > max (x@wavelength) && Re (e2) > max (x@wavelength))){
      NULL                       # wavelengths completely outside the wl. range of x
    } else {
      e1 <- .getindex (x, Re (e1)) + Im (e1)
      e2 <- .getindex (x, Re (e2)) + Im (e2)

      if (e1 <= 0 || e2 <= 0|| e1 > length (x@wavelength) || e2 > length (x@wavelength))
        warning ("wl2i: formula yields indices outside the object.")

      seq (e1, e2)
    }
  }

  .conv.range <- function (range){
    if (is.numeric (range)){
      .getindex (x, range, extrapolate = FALSE)
    } else
    eval (range)
  }

  if (is.list (wavelength))
    unlist (lapply (wavelength, .conv.range))
  else (.conv.range (wavelength))
}
