###-----------------------------------------------------------------------------
###
### spc.identify
###
###

spc.identify <- function (x, y = NULL, wavelengths = NULL, ispc = NULL, ...){
  if (is.list (x)) {
    if (is.null (wavelengths))
      wavelengths <- x$wavelengths
    if (is.null (y))
      y <- x$y
    x <- x$x
  }

  if ((length (x) != length (y)) | (length (x) != length (wavelengths)))
    stop ("x, y, and wavelength need to have the same length.")

  if (is.null (ispc))
    ispc <- row (y)
  else
    ispc <- ispc[row(y)]

  i <- identify (x, y,
                 labels = paste (ispc, format (wavelengths, digits = 4),
                   sep = ", "),
                 ...
                 )

  data.frame (ispc = ispc [i],
              wavelengths = wavelengths [i],
              spc = y [i])
}


