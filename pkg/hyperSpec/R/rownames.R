###-----------------------------------------------------------------------------
###
###  rownames
###
###

setMethod ("rownames", "hyperSpec", function (x, do.NULL = TRUE, prefix = "row"){
  validObject (x)
  
  rownames (x@data, do.NULL = do.NULL, prefix = prefix)
})

###-----------------------------------------------------------------------------
###
###  rownames <-
###
###

setReplaceMethod ("rownames", "hyperSpec", function (x, value){
  validObject (x)
  
  rownames (x@data) <- value
  .logentry (x, short = "rownames<-")
})

