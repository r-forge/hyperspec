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



###-----------------------------------------------------------------------------
###
### extractname - extracting with $
###

setMethod ("$", "hyperSpec", function (x, name){
  validObject (x)
  
  if (name == ".") ## shortcut
    x@data [, , drop = FALSE]
  else if (name == "..")
    x@data[, -match ("spc", colnames (x@data)), drop = FALSE]
  else
    x@data[[name]]
})


###-----------------------------------------------------------------------------
###
### extractsqsq - extracting with [[
###
setMethod ("[[", "hyperSpec", function (x, i, j, l, ...,
                                        wl.index = FALSE,
                                        drop = FALSE){
  validObject (x)

  x <- .extract (x, i, j, l, ..., wl.index = wl.index)

  if (missing (j))
    unclass (x@data$spc[,, drop = drop]) # removes the "AsIs"
  else {
    x@data[,, drop = drop]
  }
})

