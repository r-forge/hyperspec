##' Number of Data Points per Spectrum
##'
##' The number of data points per spectrum (number of wavelengths, number of columns in the spectra
##' matrix) 
##' 
##' @rdname ncol
##' @param x hyperSpec object
##' @return number of data points per spectrum
##' @export 
##' @examples
##' nwl (chondro)

nwl <- function (x){
  .is.hy (x)
  validObject (x)
  
  ncol (x@data$spc)
}


