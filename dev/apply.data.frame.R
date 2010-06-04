apply.df.with.matrix <- function (X, MARGIN, FUN, ...){
  if (MARGIN == 1){ ## nothing special here
    result <- apply (X, MARGIN, FUN, ...)
  } else {
    
    matrixcol <- sapply (X, is.matrix) # arrays are not allowed anyways: they produce corrupt
                                               # data.frames
    ncol <- rep (1, ncol (X))
    ncol [matrixcol] <- sapply (X [matrixcol], ncol)
    nozerocol <- ncol > 0
    matrixcol [! nozerocol] <- FALSE
    
    tmp <- apply (X [nozerocol],  MARGIN, FUN, ...) # as.matrix fails for columns containing a
                                                    # zero-column matrix. hyperSpec may produce such
                                                    # data.frames.
    lastcol <- cumsum (ncol)
    firstcol  <- lastcol - ncol + 1
    
    if (is.list (tmp)) {                 # it may happen that we get back a list!
      result <- tmp
    } else {
      if (is.null (dim (tmp)))
        dim (tmp) <- c (1, length (tmp))
      
      result <- X [rep (1, nrow (tmp)), ]

      ## first do all the normal columns
      result [, ! matrixcol & nozerocol] <- tmp [, firstcol [! matrixcol & nozerocol]]

      ## there may be a fancier (faster?) way to do this, but I assume the number of matrix columns
      ## to be rather low
      for (col in which (matrixcol)) {
        result [, col] <- tmp [, firstcol [col] : lastcol [col], drop = FALSE]
        colnames (result [[col]]) <- colnames (X [[col]])
      }
        
      ## finally a candy
      rownames (result) <- rownames (tmp)
    }
  }

  result
}

