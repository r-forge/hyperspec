##' Spiky cartilage spectra
##'
##' A selection of Raman spectra, the first half with spikes, the second half
##' contains also spectra without spikes.
##' 
##' @name cartilage
##' @docType data
##' @format The data sets consists of 100 spectra with 1272 data points each.
##' location on the sample is recorded in columns \code{x} and \code{y}. 
##' @author A. Bonifacio and C. Beleites 
##' @keywords datasets
##' @examples
##' 
##' cartilage
##' plot (sample (cartilage, 10))
##' plotvoronoi (cartilage, func = max, col.regions = alois.palette)
NULL
