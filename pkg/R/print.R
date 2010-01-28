###-----------------------------------------------------------------------------
###
###  print
###

setMethod ("print", "hyperSpec", function (x, log = FALSE, ...){
  validObject (x)
  cat (as.character (x, log = log, ...), sep ="\n")
  invisible (x)
})



