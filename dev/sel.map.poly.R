##' Interactively select a polygon (grid graphics) and highlight points
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}).

####' @param pch symbol to display the points of the polygon
####' @param size size for polygon point symbol
####' @param ... further arguments for \code{\link[grid]{grid.points}} and
####' \code{\link[grid]{grid.lines}}

##' @return array of indices for points within selected polygon
##' @author Claudia Beleites, Sebastian Mellor
##' @seealso \code{\link[grid]{grid.locator}}
##' @seealso \code{\link[hyperspec]{sel.poly}}
##' @export
##' @keywords iplot, hyperspec

sel.map.poly <- function (data, pch = 19, size = 0.3, ...){

  plot (data, "map")
  #poly <- sel.poly(pch=pch, size=size, ...)
  #library(sp)
  #pts <- point.in.polygon (chondro$x, chondro$y, poly [, 1], poly [, 2]) #mode.checked=F
  #ind <- which ((pts>0), arr.ind=TRUE)
  
  #ind
}

