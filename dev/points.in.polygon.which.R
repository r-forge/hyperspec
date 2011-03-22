##' Wrapper for points.in.polygon ato return indices only

points.in.polygon.which <- function (hyperspec, polygon.matrix){
  
  require("sp") # should check for package first
  ## CB: you have to check whether this was successful!, if you want to throw an error, use library
  ## ()
  
  pts <- point.in.polygon (hyperspec$x, hyperspec$y, 
                           polygon.matrix [, 1], polygon.matrix [, 2]) # mode.checked=F
  ind <- which ((pts>0), arr.ind=TRUE)
  ## CB: why do you use arr.ind?
  ## CB: why do you use which?
  
  ind
}

## Usage:
## > plot (chondro, "map")
## > poly <- sel.poly()
## > points.in.polygon.which (chondro, poly)
