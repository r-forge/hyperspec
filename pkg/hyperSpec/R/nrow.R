###-----------------------------------------------------------------------------
###
###  nrow
###
###

setMethod ("nrow", "hyperSpec", function (x){
  validObject (x)

  nrow (x@data)
})
