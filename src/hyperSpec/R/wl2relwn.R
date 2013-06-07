##' Convert a hyperSpec object's wavelegth axis from wavelength in nm to Raman relative wavenumbers in 1/cm.
##'
##' @title Wavelength to Raman shift conversion.
##' @param spc hyperSpec object with wavelength axis holding wavelength in nm.
##' @param laser wavelength of the excitation laser in nm
##' @return hyperSpec object with wavelength axis holding Raman shift (relative wavenumbers) in 1/cm
##' @author Claudia Beleites
##' @seealso \code{\link{wlcal.Kaiser}}
##' @export 
wl2relwn <- function (spc, laser) {
  wl (spc, expression (Delta * tilde (nu) / cm^-1)) <- 1e7 / laser - 1e7 / spc@wavelength
 
  spc
}
