###-----------------------------------------------------------------------------
###
### extractsqsq - extracting with [[
###
setMethod ("[[", "hyperSpec", function (x, i, j, l, ...,
                                        wl.index = FALSE,
                                        drop = FALSE){
  validObject (x)

  ## check wheter a index matrix is used
  if (! missing (i) && is.matrix (i)){
    if (! is.logical (i) && ! (is.numeric (i) && ncol (i) == 2))
          stop ("Index matrix i  must either be logical of the size of x$spc,",
                "or a n by 2 matrix.")

    if (is.numeric (i) && ! wl.index) 
        i [, 2] <- .getindex (x, i [, 2], extrapolate = FALSE)

    x@data$spc [i]                    # return value
  } else {                              # index by row and columns
    x <- .extract (x, i, j, l, ..., wl.index = wl.index)
    if (missing (j))
      unclass (x@data$spc[,, drop = drop]) # retrun value; removes the "AsIs"
    else {
      x@data[,, drop = drop]            # return value
    }
  }
})

