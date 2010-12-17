###-----------------------------------------------------------------------------
###
### spc.identify
###
###

spc.identify <- function (x, y = NULL, wavelengths = NULL, ispc = NULL,
                          tol.wl = diff (range (x)) / 200, 
                          tol.spc = diff (range (y)) / 50,
                          point.fn = spc.point.max, # function to find the maximum
                          formatter = spc.label.default, # NULL: suppress labels
                          ..., cex = 0.7, adj = c (0, 0.5), srt = 90, # for the label text
                          warn = TRUE){
  if (is.list (x)) {
    if (is.null (wavelengths))
      wavelengths <- x$wavelengths
    if (is.null (y))
      y <- x$y
    x <- x$x
  }

  debuglevel <- hyperSpec.getOption ("debuglevel")

  if ((length (x) != length (y)) | (length (x) != length (wavelengths)))
    stop ("x, y, and wavelength need to have the same length.")

  if (is.null (ispc))
    ispc <- row (y)
  else
    ispc <- ispc[row(y)]
  
  pts <- data.frame (ispc = rep (NA, 50), wl = NA, spc = NA)
  pos <- 1
  
  while (! is.null (tmp <- locator (n = 1))){
    wl <- approx (x, wavelengths, tmp$x, rule = 2)$y # return wl_min / wl_max for outside pts.

    if (debuglevel == 2L) {
      points (tmp$x, tmp$y, pch = ".", col = "red")
      rect (tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
            border = "red", col = NA)
    }

    i.window <- wavelengths >= wl - tol.wl & # window to search for the closest spectrum
                wavelengths <= wl + tol.wl &
                y >= tmp$y - tol.spc &
                y <= tmp$y + tol.spc

    if (! any (i.window)){
      if (warn)
        warning ("No spectra in specified window.")
      else
        pos <- pos + 1

      if (debuglevel == 1L) {
        points (tmp$x, tmp$y, pch = ".", col = "red")
        rect (tmp$x - tol.wl, tmp$y - tol.spc, tmp$x + tol.wl, tmp$y + tol.spc,
              border = "red", col = NA)
      }

    } else {
    
      ## find spectrum closest to clicked point.
      ## x and y distances are scaled according to tolerance.
      tmp <- ((wl - wavelengths [i.window]) / tol.wl)^2 +
            ((tmp$y - y [i.window]) / tol.spc)^2 
      tmp <- which (i.window) [which.min (tmp)]

      pts [pos, "ispc"] <- ispc [tmp]   # closest spectrum;
                                        # this will grow the data.frame if necessary
                                        # no time concern with hand-clicked points

      ## search for the max (min) of spectrum pt within tmp$x +- tol.wl
      i.window <- which (ispc == ispc [tmp] &
                         wavelengths >= wl - tol.wl &
                         wavelengths <= wl + tol.wl)
      
      pts [pos, 2 : 3] <- point.fn (wl = wavelengths [i.window],
                                    spc = y [i.window],
                                    wlclick = wl)
      
      ## label the point
      if (! is.null (formatter)){
        lab <- formatter (pts [pos, 1], pts [pos, 2], pts [pos, 3])
        
        text (approx (wavelengths, x, pts [pos, 2], rule = 2),
              pts [pos, 3], labels = lab, cex = cex, adj = adj, srt = srt, ...)
      }

      pos <- pos + 1
    }
    

  }

  pts [seq_len (pos - 1),]
}

spc.point.max <- function (wl, spc, wlclick){
  i <- which.max (spc)
  c (wl = wl [i], spc = spc [i])
}

spc.point.default <- function (wl, spc, wlclick){
  i <- round (approx (wl, seq_along (wl), wlclick, rule = 2)$y)
  c (wl = wl [], spc = spc [i])
}

spc.point.min <- function (wl, spc, wlclick){
  i <- which.min (spc)
  c (wl = wl [i], spc = spc [i])
}

spc.point.sqr <- function (wl, spc, wlclick, delta = 1L){
  i <- which.max (spc) 

  ## points (wl [i], spc [i])              
  if (i > 1L && i < length (wl)) {
    i <- i + (-delta : delta)
    i <- i %in% seq_along (wl)          # make sure the indices exist
    
    p <- outer (wl [i], 0 : 2, "^")     # Vandermonde matrix
    p <- qr.solve (p, spc [i])

    i <- -p [2] / p [3] / 2

    ## lines (wl, outer (wl, 0 : 2, "^") %*% p, col = "red") 
    c (wl = i, spc = sum (p * c(1, i, i^2)))

  } else {

    c (wl = wl [i], spc = spc [i])

  }
}

spc.label.default <- function (ispc, wl, spc, digits = 3){
  sprintf(" %i, %s ", ispc, format (wl, digits = digits))
}

spc.label.wlonly <- function (ispc, wl, spc, digits = 3){
  sprintf(" %s ", format (wl, digits = digits))
}




