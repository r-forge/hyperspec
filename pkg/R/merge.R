.merge <- function (x, y,
                    by = setdiff (intersect(colnames(x), colnames(y)), "spc"),
                    by.x = by, by.y = by,
                    ...){
  force (by)
  force (by.x)
  force (by.y)

  if (any (grepl ("^spc$", by.x))){
    by.x <- setdiff (by.x, "spc")
    warning ('"spc" removed from by.x')
  }
  
  if (any (grepl ("^spc$", by.y))){
    by.y <- setdiff (by.y, "spc")
    warning ('"spc" removed from by.y')
  }
  
  x$.nx <- seq_len (nrow (x))
  y$.ny <- seq_len (nrow (y))

  x.spc <- match ("spc", colnames (x))
  y.spc <- match ("spc", colnames (y))

  tmp <- merge (x@data [, -x.spc], y@data [, -y.spc], by.x = by.x, by.y = by.y, ...)

  spc.x <- matrix (NA, nrow = nrow (tmp), ncol = nwl (x))
  spc.x [! is.na (tmp$.nx),] <- x@data [tmp$.nx[! is.na (tmp$.nx)], x.spc]
  
  spc.y <- matrix (NA, nrow = nrow (tmp), ncol = nwl (y))
  spc.y [! is.na (tmp$.ny),] <- y@data [tmp$.ny[! is.na (tmp$.ny)], y.spc]

  tmp$spc <- I (cbind (spc.x, spc.y))
  
  x@data <- tmp
  x@wavelength <- c (x@wavelength, y@wavelength)

  x
}

setMethod ("merge", signature = c (x = "hyperSpec", y = "hyperSpec"),
           function (x, y, ..., short = "merge", user = NULL, date = NULL){
             validObject (x)
             validObject (y)

             tmp <- .merge (x, y, ...)

             if (nrow (tmp) == 0 && nrow (x) > 0 && nrow (y) > 0)
               warning ("Merge results in 0 spectra.")
             
             .logentry (tmp, short = short, long = list (x = as.character (y), ...),
                        user = user, date = date)
           }
           )
