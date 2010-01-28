####-----------------------------------------------------------------------------
####
####  read.ENVI.Nicolet - read ENVI files written by Nicolet instruments
####  

read.ENVI.Nicolet <- function (..., 
		# file headerfile, header
		x = NA, y = NA, # NA means: use the specifications from the header file if possible
		log = list (),
		keys.hdr2log = TRUE,
		nicolet.correction = FALSE) {
	
	log <- modifyList (list (short = "read.ENVI.Nicolet", 
									 long = list (call = match.call ())),
							 log)
	
	if (! isTRUE (keys.hdr2log))
		keys.hdr2log <- unique (c ("description", "z plot titles", "pixel size", keys.hdr2log))
	
	spc <- read.ENVI (..., keys.hdr2log = keys.hdr2log,
			x = if (is.na (x)) 0 : 1 else x,
			y = if (is.na (y)) 0 : 1 else y,
			log = log)
	
	header <-spc@log$long.description [[1]] 
	
	### From here on processing the additional keywords in Nicolet's ENVI header
	
	## z plot titles (Nicolet) ------------------------------------------------------------------------
	# default labels
	label <- list (x = expression (`/` (x, micro * m)),
						y = expression (`/` (y, micro * m)),
						spc = 'I / a.u.',
						.wavelength = expression (tilde (nu) / cm^-1))		

	if (!is.null (header$'z plot titles')){
		pattern <- "^[[:blank:]]*([[:print:]^,]+)[[:blank:]]*,.*$"
		tmp <- sub (pattern, "\\1", header$'z plot titles')
		
		if (grepl ("Wavenumbers (cm-1)", tmp, ignore.case = TRUE))
			label$.wavelength <- expression (tilde (nu) / cm^(-1))
		else
			label$.wavelength <- tmp
		
		pattern <- "^[[:blank:]]*[[:print:]^,]+,[[:blank:]]*([[:print:]^,]+).*$"
		tmp <- sub (pattern, "\\1", header$'z plot titles')
		if (grepl ("Unknown", tmp, ignore.case = TRUE))
			label$spc <- "I / a.u."
		else
			label$spc <- tmp
	}
	spc@label <- modifyList (label, spc@label)
		
	## set up spatial coordinates -------------------------------------------------------------------
	## look for x and y in the header only if x and y are NULL
	## they are in decription + pixel size for Nicolet ENVI 
	
	p.description <- "^Spectrum position [[:digit:]]+ of [[:digit:]]+ positions, X = ([[:digit:].-]+), Y = ([[:digit:].-]+)$"
	p.pixel.size  <- "^[[:blank:]]*([[:digit:].-]+),[[:blank:]]*([[:digit:].-]+).*$"

	if (is.na (x) && is.na (y) &&
			! is.null (header$description)  && grepl (p.description, header$description ) &&
			! is.null (header$'pixel size') && grepl (p.pixel.size,  header$'pixel size')) {
		
		x [1] <- as.numeric (sub (p.description, "\\1", header$description))
		y [1] <- as.numeric (sub (p.description, "\\2", header$description))
		
		x [2] <- as.numeric (sub (p.pixel.size, "\\1", header$'pixel size'))
		y [2] <- as.numeric (sub (p.pixel.size, "\\2", header$'pixel size'))
		
		# it seems that at least for some files the step size is given in mm while the offset is in μm...
		if (nicolet.correction) { 
			x [2] <- x [2] * 1000
			y [2] <- y [2] * 1000
		}
		
		x <- x [2] * spc$x + x [1]
		if (! any (is.na (x)))
			spc@data$x <- x
		
		y <- y [2] * spc$y + y [1]
		if (! any (is.na (y)))
			spc@data$y <- y
	}
	spc
}

