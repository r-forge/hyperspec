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
