###-----------------------------------------------------------------------------
###
###  print
###

setMethod ("print", "hyperSpec", function (x, log = FALSE, ...){
  validObject (x)
  cat (as.character (x, log = log, ...), sep ="\n")
  invisible (x)
})

###-----------------------------------------------------------------------------
###
###  show
###

setMethod ("show", "hyperSpec", function (object){
  print (object)
  invisible (NULL)
})

###-----------------------------------------------------------------------------
###
###  summary
###

setMethod ("summary", "hyperSpec", function (object, log = TRUE, ...){
  print (object, log = log, ...)
})


