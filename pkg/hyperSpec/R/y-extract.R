###-----------------------------------------------------------------------------
###
### .extract - internal function doing the work for extracting with [] and [[]]
###


.extract <- function (x, i, j, l,
                      ...,
                      wl.index = FALSE
                      ){
  if (! missing (i))
    x@data <- x@data[i,, drop = FALSE]

  if (!missing (j))
    x@data <- x@data[, j, drop = FALSE]

  if (!missing (l)) {
    if (is.null (x@data$spc))
      warning ("Selected columns do not contain specta. l ignored.")
    else {
      if (!wl.index)
        l <- wl2i (x, l)

      x@data$spc <- x@data$spc[,l, drop = FALSE]
      .wl (x) <- x@wavelength[l]
    }
  }

  x
}


