###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
###  C. Beleites
###

setMethod ("initialize", "hyperSpec",
		function (.Object, spc = NULL, data = NULL,
				wavelength = NULL, label = NULL, log = NULL,
				## ...,
				short = "initialize", user = NULL, date = NULL){
			long <- list ()
			
			if (is.null (data))
				.Object@data <- data.frame (spc = I (matrix (NA, 0, 0)))
			else {
				.Object@data <- data
				long$data <- .paste.row (.Object@data, val = TRUE)
			}
			
			if (!is.null (spc)){
				if (is.numeric (spc) && !is.matrix (spc))
					spc <- as.matrix (t (spc)) # make a 1 row matrix
				
				if (nrow (.Object) == nrow (spc)){
					
					if (!is.null (.Object@data$spc))
						warning ("data$spc replaced by spc.")
					
					.Object@data$spc <- I (as.matrix (spc))
					
				} else if (nrow (.Object) == 0) {
					.Object@data <- data.frame (spc = I (as.matrix (spc)))
				} else {
					stop ("data and spc need to have the same number of rows.")
				}
				
				long$spc <- .paste.row (spc, val = TRUE)
			}
			
			if (is.null (wavelength)){
				if (is.null (colnames (.Object@data$spc)))
					colnames (.Object@data$spc) <- wl.seq (.Object)
					
				.wl (.Object) <- as.numeric (colnames (.Object@data$spc))
			} else {
				.wl (.Object) <- wavelength
				long$wavelength <- wavelength
			}
			
			if (any (is.na (.Object@wavelength)))
				.Object@wavelength <- wl.seq (.Object)
			
			if (is.null (label) || length (label) == 0){
			
				.Object@label <- vector ("list", length (colnames (.Object@data)) + 1)
				names (.Object@label) <- c(".wavelength", colnames (.Object@data))
			
			} else {
			
				label <- lapply (label, 
						function (x){	
							if (is.language (x) && ! is.expression (x))
								class (x) <- "expression"
							else if (is.character (x))
								x <- as.expression (x)
							x
						})
						
				label <- as.list (label)
				.Object@label <- label
				long$label <- label
			}
			
			if (is.null (log))
				.Object <- .logentry (.Object, short = short, long = long, user = user, date = date)
			else if (is.data.frame(log))
				.Object@log <- log
			else
				.Object <- .logentry (.Object, short = log$short, long = log$long,
						date = log$date, user = log$user)
			
			validObject (.Object)
			
			.Object
		})
