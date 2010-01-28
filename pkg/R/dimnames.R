###-----------------------------------------------------------------------------
###
###  dimnames
###
###

setMethod ("dimnames", "hyperSpec", function (x){
  validObject (x)

  list (row = rownames (x@data), data = colnames (x@data),
        wl = colnames (x@data$spc))
})

