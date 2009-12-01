####################################################################################################
###
###  read.ENVI - read ENVI files, missing header files may be replaced by list in parameter header 
###
###  * read.ENVI.Nicolet for ENVI files written by Nicolet spectrometers 
###  * adapted from caTools read.ENVI
###
###  Time-stamp: <Claudia Beleites on Monday, 2009-11-09 at 14:00:01 on cb>
###
####################################################################################################

### some helper functions ..........................................................................
.read.ENVI.header  <- function (file, headerfilename) {
  if (is.null (headerfilename)) {
    headerfilename <- paste (dirname (file), sub ("[.][^.]+$", ".*", basename (file)), sep = "/")
    tmp <- Sys.glob (headerfilename)
    headerfilename <- tmp [! grepl (file, tmp)]
      
    if (length (headerfilename) != 1)
      stop ("Cannot guess header file name")
    else
      cat (".read.ENVI.header: Guessing header file name (", headerfilename, ")\n", sep = '')
  }
    
  if (!file.exists(headerfilename)) 
    stop("Could not open header file: ", headerfilename)

  readLines (headerfilename)
}

# ...................................................................................................

.read.ENVI.split.header <- function (header) {
  ## check ENVI at beginning of file
  if (!grepl("ENVI", header[1])) 
    stop("Not an ENVI header (ENVI keyword missing)")
  else
    header <- header [-1]

  ## remove curly braces and put multi-line key-value-pairs into one line
  header <- gsub("\\{([^}]*)\\}", "\\1", header)

  l <- grep("\\{", header)
  r <- grep("\\}", header)
  
  if (length(l) != length(r)) 
    stop("Error matching curly braces in header (differing numbers).")

  if (any(r <= l)) 
    stop("Mismatch of curly braces in header.")

  header[l] <- sub("\\{", "", header[l])
  header[r] <- sub("\\}", "", header[r])

  for (i in rev(seq_along(l))) 
    header <- c(header[seq_len(l[i] - 1)],
                paste(header[l[i]:r[i]], collapse = " "),
                header[-seq_len(r[i])])

  ## split key = value constructs into list with keys as names
  header <- sapply (header, split.line, "=", USE.NAMES = FALSE)
  names (header) <- tolower (names (header))

  ## some values are numeric
  tmp <- names (header) %in% c("samples", "lines", "bands", "data type", "header offset")
  header [tmp] <- lapply (header [tmp], as.numeric)

  header
}

### .................................................................................................

.read.ENVI.bin <- function (file, header) {
  if (any (is.null (header [c("samples", "lines", "bands", "data type")]) ||
           is.na   (header [c("samples", "lines", "bands", "data type")]) ))
    stop("Error in ENVI header (required entry missing or incorrect)\n header: ",
         paste (names (header), " = ", header, collapse = ", "))

  if (header$samples <= 0)
    stop("Error in ENVI header: incorrect data size (", header$samples, ")")
  if (header$lines <= 0)
    stop("Error in ENVI header: incorrect data size (", header$lines, ")")
  if (header$bands <= 0)
    stop("Error in ENVI header: incorrect data size (", header$bands, ")")
  
  if (!(header$`data type` %in% c(1 : 5, 9, 12))) 
    stop("Error in ENVI header: data type incorrect or unsupported (", header$`data type`,")")

  if (is.null (header$`byte order`)){
    header$`byte order` <- .Platform$endian
    cat (".read.ENVI.bin: 'byte order' not given or incorrect. Guessing '", .Platform$endian, "'\n", sep = '')
  }
  if (! header$`byte order` %in% c ("big", "little", "swap")) {
    header$`byte order` <- as.numeric (header$`byte order`)
    if (! header$`byte order` %in% 0 : 1) {
      header$`byte order` <- .Platform$endian
      warning ("byte order not given or incorrect. Guessing '", .Platform$endian, "'")
    } else if (header$`byte order` == 0)
      header$`byte order` <- "little"
    else 
      header$`byte order` <- "big"
  }

  n <- header$samples * header$lines * header$bands

  if (!file.exists(file)) 
    stop("Could not open binary file: ", file)

  f <- file (file, "rb")
  if (! is.null (header$`header offset`)) 
    readBin(f, raw(), n = header$`header offset`)
  
  switch(header$`data type`,
         spc <- readBin(f, integer(), n = n, size =  1, signed = FALSE),
         spc <- readBin(f, integer(), n = n, size =  2, endian = header$`byte order`),
         spc <- readBin(f, integer(), n = n, size =  4, endian = header$`byte order`),
         spc <- readBin(f, double(),  n = n, size =  4, endian = header$`byte order`),
         spc <- readBin(f, double(),  n = n, size =  8, endian = header$`byte order`),
         , # 6 unused
         , # 7 unused
         , # 8 unused
         spc <- readBin(f, complex(), n = n, size = 16, endian = header$`byte order`),
         , # 10 unused
         , # 11 unused
         spc <- readBin(f, integer(), n = n, size =  2, endian = header$`byte order`, signed = FALSE)
         )
  
  close(f)

  if (is.null (header$interleave))
    header$interleave <- "bsq"    # de
  
  switch (tolower (header$interleave),
          bil = {dim (spc) <- c(header$samples, header$bands, header$lines); spc <- aperm(spc, c(3, 1, 2))},
          bip = {dim (spc) <- c(header$bands, header$samples, header$lines); spc <- aperm(spc, c(3, 2, 1))},
          bsq = {dim (spc) <- c(header$samples, header$lines, header$bands); spc <- aperm(spc, c(2, 1, 3))},
          stop ("Unknown interleave (", header$interleave, ", should be one of 'bsq', 'bil', 'bip')")
          )

  dim (spc) <- c (header$samples * header$lines, header$bands)

  spc
}

# ..................................................................................................

read.ENVI <- function (file = stop ("read.ENVI: file name needed"), headerfile = NULL, 
							  header = list (), 
							  keys.hdr2data = FALSE, keys.hdr2log = TRUE,
                       x = 0 : 1, y = x, 
                       wavelength = NULL, label = list (), log = list ()) {
  force (y)				
  
  if (! file.exists (file))
	  stop ("File not found:", file)
						  
  tmp <- .read.ENVI.header (file, headerfile)
  tmp <- .read.ENVI.split.header (tmp)
  header <- modifyList (tmp, header)  

  ## _no_ capital letters here: .read.ENVI.split.header produces lowercase names
  recognized.keywords <- c("samples", "lines", "bands", "data type", "header offset", 
                           "interleave", "byte order", "wavelength")

  ## read the binary file
  spc <- .read.ENVI.bin (file, header)

  ## wavelength should contain the mean wavelength of the respective band
  if (! is.null (header$wavelength)) {
    header$wavelength <- as.numeric (unlist (strsplit (header$wavelength, "[,;[:blank:]]+")))

    if (! any (is.na (header$wavelength)) && is.null (wavelength))
      wavelength <- header$wavelength
  } 
  
  ## set up spatial coordinates
  x <- rep (seq (0, header$samples - 1), each = header$lines)   * x [2] + x [1]
  y <- rep (seq (0, header$lines   - 1),        header$samples) * y [2] + y [1]
  
  ## header lines => extra data columns or log entries
  extra.data <- header [keys.hdr2data]
  
  log <- modifyList (list (short = "read.ENVI", 
				               long = list (call = match.call (),
											       header = getbynames (header, keys.hdr2log))),
							log)
  
  if (length (extra.data) > 0) {
	  extra.data <- lapply (extra.data, rep, length.out = length (x))
	  data <- data.frame (x = x, y = y, extra.data)
  } else {
	  data <- data.frame (x = x, y = y)
  }

  ## finally put together the hyperSpec object
  new ("hyperSpec", spc = spc, data = data, 
       wavelength = wavelength, label = label, log = log)
}

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

