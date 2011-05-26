###-----------------------------------------------------------------------------
###
### .getindex
###
###
## does the acual work of looking up the index
## extrapolate = TRUE returns first resp. last index for wavelength outside hyperSpec@wavelength.
## extrapolate = FALSE returns NA in this case

.getindex <- function (x, wavelength, extrapolate = TRUE){
    if (! extrapolate) {
        wavelength [wavelength < min (x@wavelength)] <- NA
        wavelength [wavelength > max (x@wavelength)] <- NA
    }
    tmp <- wavelength [! is.na (wavelength)]
    if (length (tmp) > 0) {
        tmp <- sapply (tmp,
                         function (x, y) which.min (abs (x  - y)),
                         x@wavelength)
        wavelength [! is.na (wavelength)] <- tmp
    }
    wavelength
}
