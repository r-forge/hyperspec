#################################################################################
###
###  read.ENVI - read ENVI files, missing header files may be replaced by list in 
###  parameter header
###
###  * read.ENVI.Nicolet for ENVI files written by Nicolet spectrometers 
###  * adapted from caTools read.ENVI
###
###  Time-stamp: <Claudia Beleites on Monday, 2009-11-09 at 14:00:01 on cb>
###  
###  Version 1.0  2009-10-20 09:14  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################


### some helper functions ...........................................................................
.read.ENVI.header  <- function (file, header) {
  if (is.null (header)) {
    header <- paste (dirname (file), sub ("[.][^.]+$", ".*", basename (file)), sep = "/")
    dummy <- Sys.glob (header)
    header <- dummy [! grepl (file, dummy)]
      
    if (length (header) != 1)
      stop ("Cannot guess header file name")
    else
      cat (".read.ENVI.header: Guessing header file name (", header, ")\n", sep = '')
  }
    
  if (!file.exists(header)) 
    stop("Could not open header file: ", header)

  readLines (header)
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
  dummy <- names (header) %in% c("samples", "lines", "bands", "data type", "header offset")
  header [dummy] <- lapply (header [dummy], as.numeric)

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

# ...................................................................................................

read.ENVI <- function (file = stop ("read.ENVI: file name needed"), header = NULL,
                       x = 0 : 1, y = x,
                       wavelength = NULL, label = NULL, log = NULL) {
  if (!is.list (header)) {
    header <- .read.ENVI.header (file, header)
    header <- .read.ENVI.split.header (header)
  }

  ## split header into known and unknown key-value-pairs
  ## _no_ capital letters here: .read.ENVI.split.header produces lowercase names
  recognized.keywords <- c("samples", "lines", "bands", "data type", "header offset", 
                           "interleave", "byte order", "wavelength")
  unknown <- header [! names (header) %in% recognized.keywords]
  header <- header [names (header) %in% recognized.keywords]

  ## read the binary file
  spc <- .read.ENVI.bin (file, header)

  ## wavelength should contain the mean wavelength of the respective band
  if (! is.null (header$wavelength)) {
    header$wavelength <- as.numeric (unlist (strsplit (header$wavelength, "[,;[:blank:]]+")))

    if (! any (is.na (header$wavelength)) && is.null (wavelength))
      wavelength <- header$wavelength
    
  } 
  
  ## set up spatial coordinates
  ## y must be first as it defaults to x
  y <- rep (seq (0, header$lines   - 1),        header$samples) * y [2] + y [1]
  x <- rep (seq (0, header$samples - 1), each = header$lines)   * x [2] + x [1]

  ## unknown header lines end up as extra data columns
  if (length (unknown) > 0)
    data <- data.frame (x = x, y = y, unknown)
  else
    data <- data.frame (x = x, y = y)

  ## finally put together the hyperSpec object
  new ("hyperSpec", spc = spc, data = data, 
       wavelength = wavelength, label = label, log = log)
}

###-----------------------------------------------------------------------------
###
###  read.ENVI.Nicolet
###  
###  read ENVI files written by Nicolet instruments
###  parameter nicolet.correction causes 
###  

read.ENVI.Nicolet <- function (file = stop("read.ENVI: file name needed"),
                               header = NULL, x = c (NA, NA), y = c (NA, NA),
                               wavelength = NULL, label = list (), log = NULL,
                               nicolet.correction = FALSE) {

  ## split header into known and unknown key-value-pairs
  ## Nicolet seems to save the position of the first spectrum in microns, and the pixel size in mm.
  ## maybe.nicolet helps guessing whether this is an ENVI saved by Nicolet
  maybe.nicolet <- 0

  if (!is.list (header)) {
    header <- .read.ENVI.header (file, header)
    header <- .read.ENVI.split.header (header)
  }

  recognized.keywords <- c("samples", "lines", "bands", "data type", 
                           "header offset", "interleave", "byte order", "wavelength",
                           "description", "z plot titles", "pixel size" # these are for Nicolet
                           )
  
  unknown <- header [! names (header) %in% recognized.keywords]
  header <- header [names (header) %in% recognized.keywords]

  ## read the binary file
  spc <- .read.ENVI.bin (file, header)

  ## wavelength should contain the mean wavelength of the respective band ---------------------------
  ## the pattern in read.ENVI works for Nicolet files as well
  if (! is.null (header$wavelength)) {
    header$wavelength <- as.numeric (unlist (strsplit (header$wavelength, "[,;[:blank:]]+")))

    if (! any (is.na (header$wavelength)) && is.null (wavelength))
      wavelength <- header$wavelength
     
  }

  ## z plot titles (Nicolet) ------------------------------------------------------------------------
  if (!is.null (header$'z plot titles')){
    if (is.null(label))
      label <- list ()
    
    if (is.null (label$.wavelength)){
      pattern <- "^[[:blank:]]*([[:print:]^,]+)[[:blank:]]*,.*$"
      if (grepl (pattern, header$'z plot titles')) {
        dummy <- sub (pattern, "\\1", header$'z plot titles')
        if (grepl ("Wavenumbers (cm-1)", dummy, ignore.case = TRUE))
          label$.wavelength <- expression (tilde (nu) / cm^(-1))
        else
          label$.wavelength <- dummy
      }
      if (is.null (label$spc)){
        pattern <- "^[[:blank:]]*[[:print:]^,]+,[[:blank:]]*([[:print:]^,]+).*$"
        if (grepl (pattern, header$'z plot titles')) {
          dummy <- sub (pattern, "\\1", header$'z plot titles')
          if (grepl ("Unknown", dummy, ignore.case = TRUE))
            label$spc <- "I / a.u."
          else
            label$spc <- dummy
        }
      }
    }
  }
  
  ## set up spatial coordinates
  ## look for x and y in the header only if x and y are NULL
  ## they are in decription + pixel size for Nicolet ENVI -------------------------------------------

  p.description <- "^Spectrum position [[:digit:]]+ of [[:digit:]]+ positions, X = ([[:digit:].-]+), Y = ([[:digit:].-]+)$"
  p.pixel.size  <- "^[[:blank:]]*([[:digit:].-]+),[[:blank:]]*([[:digit:].-]+).*$"
  
  if(isTRUE (all.equal (x, c (NA, NA)))) {
    if (! is.null (header$description) && grepl (p.description, header$description)) 
      x [1] <- as.numeric (sub (p.description, "\\1", header$description))

    if (! is.null (header$'pixel size') && grepl (p.pixel.size, header$'pixel size')) 
      x [2] <- as.numeric (sub (p.pixel.size, "\\1", header$'pixel size'))

    x [is.na (x)] <- c(0, 1) [is.na (x)]
  }
  
  if(isTRUE (all.equal (y, c (NA, NA)))) {
    if (! is.null (header$description) && grepl (p.description, header$description))
      y [1] <- as.numeric (sub (p.description, "\\2", header$description))

    if (! is.null (header$'pixel size') && grepl (p.pixel.size, header$'pixel size')) 
      y [2] <- as.numeric (sub (p.pixel.size, "\\2", header$'pixel size'))
    
    y [is.na (y)] <- c(0, 1) [is.na (y)]
  }

  if (nicolet.correction) {
    x [2] <- x [2] * 1000
    y [2] <- y [2] * 1000
  }

  y <- rep (seq (0, header$lines - 1  ),        header$samples) * y[2] + y[1]
  x <- rep (seq (0, header$samples - 1), each = header$lines  ) * x[2] + x[1]

  ## use default labels
  label <- modifyList (list (x = expression (`/` (x, micro * m)),
                             y = expression (`/` (y, micro * m)),
                             spc = 'A',
                             .wavelength = expression (tilde (nu) / cm^-1)
                             ),
                       label)
  
  ## this is again just as in the original read.ENVI
  if (length(unknown) > 0) 
    data <- data.frame(x = x, y = y, unknown)
  else
    data <- data.frame(x = x, y = y)

  new("hyperSpec", spc = spc, data = data, wavelength = wavelength, 
      label = label, log = log)
}
