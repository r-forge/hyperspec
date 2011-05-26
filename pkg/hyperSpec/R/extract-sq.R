###-----------------------------------------------------------------------------
###
### extractsquare - extracting with []
###

setMethod ("[", "hyperSpec", function (x, i, j, l, ..., 
                                       wl.index = FALSE, 
                                       short = "[]", date = NULL, user = NULL,
                                       drop = FALSE # drop has to be at end
                                       ){
  validObject (x)

  if (drop)
    warning ("Ignoring drop = TRUE.")

  x <- .extract (x, i, j, l, ..., wl.index = wl.index)

  if (is.null (x@data$spc)){
    x@data$spc <- matrix (NA, nrow (x@data), 0)
    x@wavelength <- numeric (0)
  }

  .logentry (x, short = short,
             long = .call.list (match.call (call = sys.call (-1))),
             date = date, user = user)
})

