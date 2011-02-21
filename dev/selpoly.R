sel.poly <- function (pch = 19, size = 0.3, ...){
  trellis.focus ()

  pts <- matrix (NA, nrow = 0, ncol = 2)
  
  repeat {
    pt <- grid.locator (unit="native")
    if (!is.null (pt)){
      pts <- rbind (as.numeric (pt))
      
      grid.points (unit (pts [pos, 1], "native"),
                   unit (pts [pos, 2], "native"), pch = pch,
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
