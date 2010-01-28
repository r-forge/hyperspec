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

