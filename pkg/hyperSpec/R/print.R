###-----------------------------------------------------------------------------
###
###  print
###

setMethod ("print", "hyperSpec", function (x, log = FALSE, range = FALSE, ...){
  validObject (x)
  cat (as.character (x, log = log, range = FALSE, ...), sep ="\n")
  invisible (x)
})



