###-----------------------------------------------------------------------------
###
### as.long.df
###
### TODO: look for other matrices/arrays to expand

as.long.df <- function (x, rownames = FALSE, wl.factor = FALSE, na.rm = TRUE) {
  chk.hy (x)
  validObject (x)

  ispc <- match ("spc", colnames (x@data))

  if (nwl (x) == 0) {
    tmp <- cbind (data.frame (.wavelength = rep (NA, nrow (x)),
                              spc = rep (NA, nrow (x))),
                  x@data [, -ispc, drop = FALSE])
  } else {
    tmp <- x@data [rep (row.seq (x), nwl (x)), -ispc, drop = FALSE]

    tmp <- cbind (data.frame (.wavelength = rep (x@wavelength, each = nrow (x)),
                              spc = as.numeric (x [[]])),
                  tmp)
    if (wl.factor){
      tmp$.wavelength <- as.factor (tmp$.wavelength)
      wl <- colnames (x@data$spc)       # there may be a fancily formatted version in the column
                                        # names
      if (is.null (wl))
        wl <- x@wavelength              # if not, use the wavelength vector
      
      levels (tmp$.wavelength) <- wl
    }
  }

  if (rownames)
    tmp <- data.frame (.rownames = as.factor (rep (rownames (x),
                         length.out = nrow (tmp))),
                       tmp)

  if (na.rm)
    tmp <- tmp [!is.na (tmp$spc), ]
  
  tmp
}

