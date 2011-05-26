###-----------------------------------------------------------------------------
###
### as.matrix
###

setMethod ("as.matrix", "hyperSpec", function (x, ...){
  validObject (x)

  unclass (x@data$spc)                  # remove the AsIs
})

