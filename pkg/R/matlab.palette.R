###-----------------------------------------------------------------------------
###
###  matlab.palette
###
###

matlab.palette <- function (n = 100, ...) {
  rev (rainbow (n, start = 0, end = 4/6, ...))
}


