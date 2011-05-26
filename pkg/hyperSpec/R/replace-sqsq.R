###-----------------------------------------------------------------------------
###
### replacing with [[<-
###

setReplaceMethod ("[[", "hyperSpec",
                  function (x, i, j, l, wl.index = FALSE,
                            short = "[[<-", user = NULL, date = NULL,
                            ..., value){
  validObject (x)

  long <- list (i = if (missing (i)) "" else i ,
                l = if (missing (l)) "" else l,
                wl.index = wl.index,
                ...,
                value = if (is (value, "hyperSpec")) as.character (value)
                else .paste.row (value, val = TRUE)
                )
  
  if (is (value, "hyperSpec")){
    validObject (value)
    value <- value@data$spc
  }
  
  ## check wheter a index matrix is used
  if (! missing (i) && is.matrix (i)){
    if (is.logical (i)) {
      x@data$spc [i] <- value
    } else if (is.numeric (i) && ncol (i) == 2) {
      if (! wl.index) {
        i [, 2] <- .getindex (x, i [, 2], extrapolate = FALSE)
        if (any (is.na (i [, 2])))
          stop ("wavelength specification outside spectral range")
      }
      x@data$spc [i] <- value
    } else
    stop ("Index matrix i  must either be logical of the size of x$spc,",
          "or a n by 2 matrix.")
  
  } else {                              # index by row and columns
    if (! missing (j))
      stop ("The spectra matrix may only be indexed by i (spectra) and l",
            " (wavelengths). j (data column) must be missing.")

    if  (!missing (l) && !wl.index)
      l <- wl2i (x, l)

    x@data$spc[i, l, ...] <- value
  }

  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
})
