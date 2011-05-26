###-----------------------------------------------------------------------------
###
###  dim
###
###

setMethod ("dim", "hyperSpec", function (x){
  validObject (x)
  c (nrow = nrow (x@data), ncol = ncol (x@data), nwl = ncol (x@data$spc))
})



