###-----------------------------------------------------------------------------
###
###  logbook
###
###

logbook <- function (x){
  .is.hy (x)
  validObject (x)

  x@log
}

###-----------------------------------------------------------------------------
###
###  logentry
###
###

logentry <- function (x, short = NULL, long = NULL, date = NULL, user = NULL){
  .is.hy (x)
  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
}
