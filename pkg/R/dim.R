###-----------------------------------------------------------------------------
###
###  ncol
###
###

setMethod ("ncol", "hyperSpec", function (x){
  validObject (x)
  ncol (x@data)
})


###-----------------------------------------------------------------------------
###
###  nrow
###
###

setMethod ("nrow", "hyperSpec", function (x){
  validObject (x)
  nrow (x@data)
})

###-----------------------------------------------------------------------------
###
###  dim
###
###

setMethod ("dim", "hyperSpec", function (x){
  validObject (x)
  c (nrow = nrow (x@data), ncol = ncol (x@data), nwl = ncol (x@data$spc))
})

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
  validObject (x)
  .logentry (x)
})


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
  .logentry (x)
})

###-----------------------------------------------------------------------------
###
###  dimnames
###
###

setMethod ("dimnames", "hyperSpec", function (x){
  validObject (x)
  list (row = rownames (x@data), data = colnames (x@data), wl = colnames (x@data$spc))
})

###-----------------------------------------------------------------------------
###
###  nwl
###
###
nwl <- function (x){
  .is.hy (x)
  validObject (x)
  
  ncol (x@data$spc)
}
