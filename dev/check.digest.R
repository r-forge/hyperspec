# utility functions for unit testing
#
#

.make.check.digest <- function (target, check.label = TRUE, check.log = FALSE){
	
	.serialize.hyperSpec <- function (obj)
		c (serialize (obj@wavelength, NULL),
			serialize (obj@data, NULL),
			if (check.label) serialize (obj@label, NULL) else NULL,
			if (check.log) serialize (obj@log, NULL) else NULL)
	

	if (is.list (target)) {
		## may be a list of hyperSpec objects	
		lapply (target, .is.hy)
		lapply (target, validObject)
		serialization <- unlist (lapply (target, .serialize.hyperSpec))
	} else {
		.is.hy (target)		
		validObject (target)
		serialization <- .serialize.hyperSpec (target)
	}

	
	digest (serialization, serialize = FALSE)
}

.print.digest.check <- function (target, check.label = TRUE, check.log = FALSE) {
	cat ("checkEquals (.make.check.digest (, check.label = ", check.label, ", check.log = ", check.log,"), \"", 
			.make.check.digest (target, check.label = check.label, check.log = check.log), "\")\n", sep = "") 
}