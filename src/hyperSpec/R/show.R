###-----------------------------------------------------------------------------
###
###  show
###

setMethod ("show", "hyperSpec", function (object){
  print (object, range = TRUE)
  invisible (NULL)
})
