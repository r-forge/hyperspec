###-----------------------------------------------------------------------------
###
###  matlab.dark.palette
###
###

matlab.dark.palette <- function (n = 100, ...) {
  pal <- rev (rainbow (n, start = 0, end = 4/6, ...))
  pal <- col2rgb(pal)
  pal ["green",] <- pal ["green",] / 2

  rgb (t (pal)/255)
}


