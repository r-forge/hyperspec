###-----------------------------------------------------------------------------
###
###  apply
###
###

setMethod ("apply", "hyperSpec", function (X, MARGIN, FUN, ...,
                                           label.wl = NULL, label.spc = NULL, new.wavelength = NULL,
                                           short = "apply", long = NULL, user = NULL, date = NULL){
  validObject (X)

  if (is.null (long))
    long <- .call.list (match.call (call = sys.call (-1)))
      
      #list (MARGIN = MARGIN, FUN = FUN, ...,
      #            call = deparse (sys.call()[])
      #            )
  if (all (MARGIN == 1 : 2)){
    X@data$spc <- do.call (FUN, list (X@data$spc, ...))
  } else {
    X@data <- .apply(X@data, MARGIN = MARGIN, FUN = FUN, ...)

    if (MARGIN == 1) {
      ## no shortcuts here: the validation will fail until the
      ## wavelength axis is adjusted
      if (!is.null (new.wavelength))
        if (is.numeric (new.wavelength))
          .wl (X) <- new.wavelength
        else {
          dots <- list (...)
          .wl (X) <- dots[[new.wavelength]]
        }
      else if (ncol (X@data$spc) != length (X@wavelength)){
        wl <- as.numeric (colnames (X@data$spc))
        if (length (wl) != ncol (X@data$spc) || any (is.na (wl)))
          .wl (X) <- seq_len (ncol (X@data$spc))
        else
          .wl (X) <- wl
      }

      if (ncol (X@data$spc) != length (X@wavelength))
        X@label$.wavelength <- NULL

    }
  }

  if (!is.null (label.wl))
    X@label$.wavelength <- label.wl

  if (!is.null (label.spc))
    X@label$spc <- label.spc

  validObject (X)

  .logentry(X, short = short, long = long, user = user, date = date)
})


