sel.poly <- function (pch = 19, size = 0.3, ...){
  trellis.focus ()

  pts <- matrix (NA, nrow = 100, ncol = 2)
  pos <- 1

  repeat {
    pt <- grid.locator (unit="native")
    if (!is.null (pt)){
      if (pos > nrow (pts))
        pts <- cbind (pts, matrix (NA, nrow = 100, ncol = 2))
      pts [pos, ] <- as.numeric (pt)
      
      grid.points (unit (pts [pos, 1], "native"),
                   unit (pts [pos, 2], "native"), pch = pch,
                   size = unit (size, "char"), gp = gpar (...))
      if (pos > 1)
        grid.lines (unit (pts [(pos - 1) : pos, 1], "native"),
                    unit (pts [(pos - 1) : pos, 2], "native"), gp = gpar (...))

      pos <- pos + 1
    } else 
    break
  }
  
  pts [seq_len (pos - 1),]
}
