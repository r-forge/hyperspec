##' Interactively select a polygon (grid graphics) and highlight points
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}). Polygon will be drawn closed. Wrapper for 
##' \code{\link{plotmap}}, \code{\link{sel.poly}}, and \code{\link[sp]{point.in.polygon}}.
##' @param data hyperSpec object for plotting map
##' @param pch symbol to display the points of the polygon for \code{\link{sel.poly}}
##' @param size size for polygon point symbol for \code{\link{sel.poly}}
##' @param ... further arguments for \code{\link[grid]{grid.points}} and
##' \code{\link[grid]{grid.lines}}
##' @return array of indices for points within selected polygon
##' @author Claudia Beleites, Sebastian Mellor
##' @seealso \code{\link[grid]{grid.locator}},  \code{\link{sel.poly}}, \code{\link{map.identify}}
##' @export
##' @keywords iplot, hyperspec

sel.map.poly <- function (data, pch = 19, size = 0.3, ...){
  print (plotmap (data))
  poly <- sel.poly(pch=pch, size=size, ...)
  
  debuglevel <- hy.getOption ("debuglevel")
  
  if (!require("sp")){
    cat("Error: SP package required for point.in.polygon ()\n")
    stop() ## SM: What other error handling solutions are you currently using?
           ##     Or should I just use library ()?
  }

  pts <- point.in.polygon (data$x, data$y, poly [, 1], poly [, 2]) #mode.checked=F

  ind <- which ((pts>0), arr.ind=TRUE)
  ## CB: why do you use arr.ind?
  ## SM: some data types need it, i.e. matrices, I kept it in incase the structure ever changed
  ## CB: why do you use which?
  ## CB: In other words: what is the advantage over just returning pts > 0 ?
  ## SM: with the polygon being drawn by hand I didn't think it important to differ points on the 
  ##     edges or vertices from those within. Your thoughts?
  
  ind
}

