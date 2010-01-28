###-----------------------------------------------------------------------------
###
###  colnames
###
###

setMethod ("colnames", "hyperSpec", function (x, do.NULL = TRUE, prefix = "col"){
  validObject (x)
  colnames (x@data, do.NULL = do.NULL, prefix = prefix)
})

###-----------------------------------------------------------------------------
###
###  colnames <-
###
###

setReplaceMethod ("colnames", "hyperSpec", function (x, value){
  validObject (x)

  colnames (x@data) <- value
  
  validObject (x)                       # necessary: $spc could be renamed!
  .logentry (x)
})
