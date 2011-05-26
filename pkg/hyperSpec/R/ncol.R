###-----------------------------------------------------------------------------
###
###  ncol
###
###

setMethod ("ncol", "hyperSpec", function (x){
  validObject (x)

  ncol (x@data)
})

