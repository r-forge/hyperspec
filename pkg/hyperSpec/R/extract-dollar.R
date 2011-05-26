###-----------------------------------------------------------------------------
###
### $
###

setMethod ("$", "hyperSpec", function (x, name){
  validObject (x)
  
  if (name == ".") ## shortcut
    x@data [, , drop = FALSE]
  else if (name == "..")
    x@data[, -match ("spc", colnames (x@data)), drop = FALSE]
  else
    x@data[[name]]
})

