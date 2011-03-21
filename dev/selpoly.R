##' Interactively select a polygon (grid graphics)
##'
##' Click the points that should be connected as polygon. Input ends with right click (see
##' \code{\link[grid]{grid.locator}}).
##' @param pch symbol to display the points of the polygon
##' @param size size for polygon point symbol
##' @param ... further arguments for \code{\link[grid]{grid.points}} and
##' \code{\link[grid]{grid.lines}}
##' @return n x 2 matrix with the points of the polygon
##' @author Claudia Beleites
##' @seealso \code{\link[grid]{grid.locator}}
##' @export 
sel.poly <- function (pch = 19, size = 0.3, ...){
  trellis.focus ()

  pts <- matrix (NA, nrow = 0, ncol = 2)
  
  repeat {
    pt <- grid.locator (unit="native")
    if (!is.null (pt)){
      pts <- rbind (pts, as.numeric (pt))
      
      grid.points (unit (tail (pts [, 1], 1), "native"),
                   unit (tail (pts [, 2], 1), "native"), pch = pch,
                   size = unit (size, "char"), gp = gpar (...))
      
      if (nrow (pts) > 1L)
        grid.lines (unit (tail (pts [, 1L], 2L) , "native"),
                    unit (tail (pts [, 2L], 2L) , "native"), gp = gpar (...))
    } else {
      break
    }
  }
  
  pts
}
