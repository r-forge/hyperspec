##' JCAMP-DX Import for Shimadzu Library Spectra
##'
##' this is a first rough import function for JCAMP-DX spectra as exported by the
##' Shimadzu GCxGC-MS library
##' @note JCAMP-DX support is incomplete and the functions may change without notice. See
##' \code{vignette ("fileio")}
##' @param filename file name and path of the .jdx file
##' @param encoding encoding of the JCAMP-DX file (used by \code{\link[base]{readLines}})
##' @param header list with manually set header values
##' @param keys.hdr2data character vector of labels (lowercase, without and dashes, blanks,
##' underscores) whose values should be tranfered into the extra data.
##' @return hyperSpec object
##' @author C. Beleites
##' @export
read.jdx <- function(filename = stop ("filename is needed"), encoding = "",
                     header = list (), keys.hdr2data = FALSE){
  jdx <- readLines (filename, encoding = encoding)

  ## start & end of spectra header and data
  spcstart <-  grep ("^##XYDATA=", jdx) + 1
  spcend <- grep ("^##END=[[:blank:]]*$", jdx) - 1
  hdrstart <- c (1, head (spcend, -1) + 1)

  stopifnot (length (spcstart) == length (spcend))
  stopifnot (all (spcstart < spcend))

  spc <- list ()

  for (s in seq_along (spcstart)){
    ## look for header data
    hdr <- modifyList (header, .jdx.readhdr (jdx [hdrstart [s] : (spcstart [s] - 1)]))

    if (s == 1L) ## file header may contain overall settings
        header <- hdr

    ## evaluate data block
    spc <- switch (hdr$xydata,
                   `(X++(Y..Y))`= .jdx.TABULAR.PAC  (hdr, jdx [spcstart [s] : spcend [s]]),
                   `(XY..XY)`   = .jdx.TABULAR.AFFN (hdr, jdx [spcstart [s] : spcend [s]]),
                   stop ("unknown JCAMP-DX data format: ", hdr$xydata)
                   )

    ## process according to header entries
    spc <- .jdx.processhdr (spc, hdr, keys.hdr2data)
  }

  if (length (spc) > 1L)  
      spc <- collapse (spc)

  spc
}

### HEADER ------------------------------------------------------------------------------------------

.jdx.readhdr <- function (hdr){
  names <- tolower (sub ("^##(.*)=.*$", "\\1", hdr))
  names <- gsub ("[[:blank:]_-]", "", names)
  
  hdr <- sub ("^##.*=[[:blank:]]*(.*)[[:blank:]]*$", "\\1", hdr)
  hdr <-   gsub ("^[\"'[:blank:]]*([^\"'[:blank:]].*[^\"'[:blank:]])[\"'[:blank:]]*$", "\\1", hdr)
  i <- grepl ("^[-.[:digit:]]*[eE]?[-.[:digit:]]*$", hdr)
  hdr <- as.list (hdr)
  hdr [i] <- as.numeric (hdr [i])
  names (hdr) <- names
  
  hdr
}

.jdx.processhdr <- function (spc, hdr, keys){

  if (hdr$jcampdx != 4.24)
      warning ("Only JCAMP-DX V 4.24 is supported.")
  
  ## hdr$xfactor and $yfactor applied by individual reading functions
    
  spc@label$.wavelength <- .jdx.xunits (hdr$xunits)
  spc@label$spc <- .jdx.yunits (hdr$yunits)

  hdr[c ("jcampdx", "xunits", "yunits", "xfactor", "yfactor", "firstx", "lastx", "npoints",
         "firsty", "xydata", "end", "deltax", "maxy", "miny")] <- NULL
  hdr <- hdr[keys]

  if (length (hdr) > 0L)
      spc@data <- cbind (spc@data, hdr)
  
  spc
}

### DATA FORMATS ------------------------------------------------------------------------------------

.jdx.TABULAR.PAC <- function (hdr, data){
  wl <- seq (hdr$firstx, hdr$lastx, length.out = hdr$npoints)
  
  y <- sub ("^[[:digit:]]+([-+].*)$", "\\1", data)
  y <- gsub ("[+]", " +", y)
  y <- gsub ("[-]", " -", y)
  y <- sub ("^ +", "", y)
  y <- strsplit (y, "[[:blank:]]+")
  y <- as.numeric (unlist (y))
  ny <- sapply (y, length)

  if (length (y) != hdr$npoints)
      stop ("mismatch between ##NPOINTS and length of Y data.")

  ## TODO: check X checkpoints
  ## x <- as.numeric (sub ("^([[:digit:]]+)[-+].*$", "\\1", data)) * hdr$xfactor
  ##  if (! all.equal (wl [c (1, head (cumsum (ny) + 1, -1))], x))

  if (! is.null (hdr$yfactor))
      y <- y * hdr$yfactor

  new ("hyperSpec", spc = y, wavelength = wl)
}

.jdx.TABULAR.AFFN <- function (hdr, data){
  data <- strsplit (data, "[[:blank:]]+")
  data <- unlist (data)
  spc <- read.txt.long  (textConnection (data),
                  cols = list (.wavelength="", spc=""),
                  header = FALSE, sep = ",")

  if (! is.null (hdr$xfactor))
      spc@wavelength <- spc@wavelength * hdr$xfactor
  
  if (! is.null (hdr$yfactor))
      spc@data$spc <- spc@data$spc * hdr$yfactor

  spc
}

### UNITS -------------------------------------------------------------------------------------------

.jdx.xunits <- function (xunits){
  if (is.null (xunits))
      NULL
  else 
      switch (tolower (xunits),
              `1/cm` = expression (tilde (nu) / cm^-1),
              micrometers = expression (`/` (lambda, micro * m)),
              nanometers  = expression (lambda / nm),
              seconds     = expression (t / s),
              xunits)
}

.jdx.yunits <- function (yunits){
  if (is.null (yunits))
      NULL
  else 
      switch (tolower (yunits),
              transmittance     = "T",
              reflectance       = "R",
              absorbance        = "A",
              `kubelka-munk`    = expression (`/` (1 - R^2, 2*R)),
              `arbitrary units` = "I / a.u.",
              yunits)
}
