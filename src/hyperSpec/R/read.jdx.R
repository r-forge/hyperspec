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

  ## BRUKAFFN.DX has ##JCAMPDX= 5.0
  if (sub ("^[[:blank:]]*##JCAMP-?DX=[[:blank:]]*([0-9.]*)[[:blank:]]*.*$", "\\1",
           jdx [grepl ("##JCAMP-?DX", jdx)]) != "4.24")
  stop ("Only a subset of JCAMP-DX v 4.24 is supported so far. ",
        "Please contact the maintainer about further advise.")

  ## start & end of spectra header and data
  spcstart <-  grep ("^[[:blank:]]*##(XYDATA=|DATA TABLE=)", jdx) + 1
      # V 4.24 uses ##XYDATA=
      # V 5.00 uses ##DATA TABLE= ..., XYDATA
      
  spcend <- grep ("^[[:blank:]]*##END=[[:blank:]]*$", jdx) - 1
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

    if (grepl ("[A-Za-z%@]", jdx[spcstart [s]]))
        stop ("SQZ, DIF, and DIFDUP forms are not yet supported.")
    
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
  names <- tolower (sub ("^[[:blank:]]*##(.*)=.*$", "\\1", hdr))
  names <- gsub ("[[:blank:]_-]", "", names)
  
  hdr <- sub ("^[[:blank:]]*##.*=[[:blank:]]*(.*)[[:blank:]]*$", "\\1", hdr)
  hdr <-   gsub ("^[\"'[:blank:]]*([^\"'[:blank:]].*[^\"'[:blank:]])[\"'[:blank:]]*$", "\\1", hdr)
  i <- grepl ("^[[:blank:]]*[-.[:digit:]]*[eE]?[-.[:digit:]]*[[:blank:]]*$", hdr)
  hdr <- as.list (hdr)
  hdr [i] <- as.numeric (hdr [i])
  names (hdr) <- names
  
  hdr
}

.jdx.processhdr <- function (spc, hdr, keys){

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

  if (is.null (hdr$firstx))  stop ("##FIRSTX= missing.")
  if (is.null (hdr$lastx))   stop ("##LASTX= missing.")
  if (is.null (hdr$npoints)) stop ("##NPOINTS= missing.")
  
  wl <- seq (hdr$firstx, hdr$lastx, length.out = hdr$npoints)

  ## remove starting X
  y <- sub ("^[[:blank:]]*[0-9.]+[[:blank:]]*([-+]?.*)$", "\\1", data)

  ## add spaces between numbers if necessary
  y <- gsub ("([0-9])([+-])", "\\1 \\2", y)

  y <- strsplit (y, "[[:blank:]]+")
  y <- as.numeric (unlist (y))

  ny <- length (y)

  if (length (y) != hdr$npoints)
      stop ("mismatch between ##NPOINTS and length of Y data.")

  ## TODO: check X checkpoints
  ## x <- as.numeric (sub ("^([[:digit:]]+)[-+].*$", "\\1", data)) * hdr$xfactor
  ##  if (! all.equal (wl [c (1, head (cumsum (ny) + 1, -1))], x))

  if (is.null (hdr$yfactor))
      hdr$yfactor <- 1
  
  y <- y * hdr$yfactor

  if (! is.null (hdr$miny) && abs (hdr$miny - min (y)) > hdr$yfactor)
      warning (sprintf ("Minimum of spectrum != MINY: difference = %0.3g (%0.3g * YFACTOR)",
                        min (y) - hdr$miny,
                        (min (y) - hdr$miny) / hdr$yfactor))

  if (! is.null (hdr$maxy) && abs (hdr$maxy - max (y)) > hdr$yfactor)
      warning (sprintf ("Maximum of spectrum != MAXY: difference = %0.3g (%0.3g * YFACTOR)",
                        max (y) - hdr$maxy,
                        (max (y) - hdr$maxy) / hdr$yfactor))

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
