##’ Class "hyperSpec"
##’ This class handles hyperspectral data sets, i.e. spatially or time-resolved
##’ spectra, or spectra with any other kind of information associated with the
##’ spectra.
##’ 
##’ The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES, NIR,
##’ IR, Raman, NMR, MS, etc.
##’ 
##’ More generally, any data that is recorded over a discretized variable, e.g.
##’ absorbance = f (wavelength), stored as a vector of absorbance values for
##’ discrete wavelengths is suitable.
##’ 
##’ 
##’ @name hyperSpec-class
##’ @docType class
##’ @author C. Beleites
##’ @seealso See the vignette "introduction" for an introduction to hyperSpec
##’   from a spectroscopic point of view.
##’ @keywords classes
##’ @examples
##’ 
##’ showClass("hyperSpec")
##’ \dontrun{vignette ("introduction")}
##’ 

setClass ("hyperSpec",
          representation = representation (
            wavelength = "numeric",     # spectral abscissa
            data = "data.frame",        # data: spectra & information related to each spectrum
            label = "list",             # labels and units of the stored 
            log = "data.frame"          # log of transformations etc.
            ),
          prototype = prototype (	
            wavelength = numeric (0),
            data = data.frame (spc = I (matrix (NA, 0, 0))),
				label = list (.wavelength = NULL, "spc" = NULL),
            log = data.frame (short.description = character (0),
              long.description = I (list ()),
              date = numeric (0),
              user = character (0)
              )),
          validity = function (object) {


##’ The Number of Rows (Spectra), Columns, and Data Points per Spectrum of an
##’ hyperSpec Object)
##’ \code{nrow} yields the number of rows in \code{x@data}, i.e. the number of
##’ spectra in the \code{hyperSpec} object.
##’ 
##’ \code{ncol} returns the number of columns in \code{x@data}. I.e. the number
##’ of columns with additional information to each spectrum (e.g. "x", "y",
##’ \dots{}) + 1 (for column \code{spc} containing the spectra).
##’ 
##’ \code{nwl} returns the number of columns in \code{x@data$spc}, i.e. the
##’ length of each spectrum.
##’ 
##’ \code{dim} returns all three values in a vector.
##’ 
##’ \code{length} is a synonym for \code{nrow}. It is supplied so that
##’ \code{seq_along (x)} returns a sequence to index each spectrum.
##’ 
##’ 
##’ @name ncol
##’ @aliases ncol,hyperSpec-method nrow,hyperSpec-method nwl
##’   dim,hyperSpec-method length,hyperSpec-method
##’ @docType methods
##’ @param x a \code{hyperSpec} object
##’ @return \code{nrow}, \code{ncol}, \code{nwl}, and \code{length},return an
##’   \code{integer}.
##’ 
##’ \code{dim} return a vector of length 3.
##’ @author C. Beleites
##’ @seealso \code{\link[base]{nrow}}, \code{\link[base]{ncol}},
##’   \code{\link[base]{dim}}, and \code{\link[base]{length}}
##’ 
##’ \code{\link{seq.hyperSpec}}, and \code{\link[base]{seq_along}},
##’ @keywords methods
##’ @examples
##’ 
##’ nrow (chondro)
##’ ncol (chondro)
##’ nwl  (chondro)
##’ dim (chondro)
##’ length (chondro)
##’ 
            ncol <- ncol (object@data$spc)
				
            if (is.null (ncol))
              ncol <- 0
            
			  	if (length (object@wavelength) != ncol)
              return ("Length of wavelength vector differs from number of data points per spectrum.")

            if (any (is.na (match (colnames (object@log),
                                   c("short.description", "long.description",  "date", "user")))))
              return ("Slot log does not have the correct columns.")

            TRUE
          }
          )

