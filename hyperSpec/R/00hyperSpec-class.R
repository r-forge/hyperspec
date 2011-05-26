#################################################################################
###
###  class definition and validity checking
###  
###  C. Beleites
###

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

