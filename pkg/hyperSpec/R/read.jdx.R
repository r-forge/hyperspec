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
##' @param \dots further parameters handed to the data import function, e.g.
##' \tabular{ll}{
##' \code{xtol} \tab tolerance for checking calculated x values against checkpoints at beginning
##'                  of line, defaults to XFACTOR\cr
##' \code{ytol} \tab tolerance for checking Y values against MINY and MAXY, defaults to YFACTOR\cr
##' }
##' @return hyperSpec object
##' @author C. Beleites
##' @export
read.jdx <- function(filename = stop ("filename is needed"), encoding = "",
                     header = list (), keys.hdr2data = FALSE, ...){
  jdx <- readLines (filename, encoding = encoding)
  
  ## BRUKAFFN.DX has ##JCAMPDX= 5.0
  if (any (sub ("^[[:blank:]]*##JCAMP-?DX=[[:blank:]]*([0-9.]*)[[:blank:]]*.*$", "\\1",
                jdx [grepl ("##JCAMP-?DX", jdx)]) != "4.24"))
     stop ("Only a subset of JCAMP-DX v 4.24 is supported so far.")

  ## start & end of spectra header and data
  hdrstart <- grep ("^[[:blank:]]*##TITLE=", jdx)
      

  spcstart <- grep ("^[[:blank:]]*##(XYDATA|DATA TABLE)=", jdx) + 1
      # V 4.24 uses ##XYDATA=
      # V 5.00 uses ##DATA TABLE= ..., XYDATA
      
  spcend <- grep ("^[[:blank:]]*##END=[[:blank:]]*$", jdx) - 1

  ## some checks
  if (length (hdrstart) == 0L) stop ("No spectra found.")
  stopifnot (length (spcstart) == length (hdrstart))
  stopifnot (length (spcstart) == length (spcend))
  stopifnot (all (hdrstart < spcstart))
  stopifnot (all (spcstart < spcend))

  spc <- list ()

  for (s in seq_along (spcstart)){
    ## look for header data
    hdr <- modifyList (header, .jdx.readhdr (jdx [hdrstart [s] : (spcstart [s] - 1)]))

    if (s == 1L) ## file header may contain overall settings
        header <- hdr

    ## evaluate data block

    if (grepl ("[A-DF-Za-df-z%@]", jdx[spcstart [s]]))
        stop ("SQZ, DIF, and DIFDUP forms are not yet supported.")
    
    spc <- switch (hdr$xydata,
                   `(X++(Y..Y))`= .jdx.TABULAR.PAC  (hdr, jdx [spcstart [s] : spcend [s]], ...),
                   `(XY..XY)`   = .jdx.TABULAR.AFFN (hdr, jdx [spcstart [s] : spcend [s]], ...),
                   stop ("unknown JCAMP-DX data format: ", hdr$xydata)
                   )

    ## process according to header entries
    spc <- .jdx.processhdr (spc, hdr, keys.hdr2data, ...)
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

  ## e.g. Shimadzu does not always save XFACTOR and YFACTOR
  if (is.null (hdr$yfactor)) hdr$yfactor <- 1
  if (is.null (hdr$xfactor)) hdr$xfactor <- 1

  hdr
}

.jdx.processhdr <- function (spc, hdr, keys, ..., ytol = hdr$yfactor){

  ## hdr$xfactor and $yfactor applied by individual reading functions

  ## check Y values
  miny <- min (spc@data$spc)
  if (! is.null (hdr$miny) && abs (hdr$miny - miny) > ytol)
      warning (sprintf ("Minimum of spectrum != MINY: difference = %0.3g (%0.3g * YFACTOR)",
                        miny - hdr$miny,
                        (miny - hdr$miny) / hdr$yfactor))

  maxy <- max (spc@data$spc)
  if (! is.null (hdr$maxy) && abs (hdr$maxy - maxy) > ytol)
      warning (sprintf ("Maximum of spectrum != MAXY: difference = %0.3g (%0.3g * YFACTOR)",
                        maxy - hdr$maxy,
                        (maxy - hdr$maxy) / hdr$yfactor))

  
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

.jdx.TABULAR.PAC <- function (hdr, data, ..., xtol = hdr$xfactor){

  ## regexp for numbers including scientific notation
  .PATTERN.number <- "[0-9]*[.]?[0-9]*([eE][+-]?[0-9]+)?"
  
  if (is.null (hdr$firstx))  stop ("##FIRSTX= missing.")
  if (is.null (hdr$lastx))   stop ("##LASTX= missing.")
  if (is.null (hdr$npoints)) stop ("##NPOINTS= missing.")
  
  wl <- seq (hdr$firstx, hdr$lastx, length.out = hdr$npoints)

  ## remove starting X
  y <- sub (paste0 ("^[[:blank:]]*", .PATTERN.number, "[[:blank:]]*(.*)$"), "\\2", data)

  ## add spaces between numbers if necessary
  y <- gsub ("([0-9.])([+-])", "\\1 \\2", y)

  y <- strsplit (y, "[[:blank:]]+")
  ny <- sapply (y, length)

  y <- as.numeric (unlist (y))


  if (length (y) != hdr$npoints)
      stop ("mismatch between ##NPOINTS and length of Y data.")

  ## X checkpoints
  x <- sub (paste0 ("^[[:blank:]]*(", .PATTERN.number, ")[[:blank:]]*.*$"), "\\1", data)
  x <- as.numeric (x) * hdr$xfactor
  diffx <- abs (wl [c (1, head (cumsum (ny) + 1, -1))] - x)
  if (any (diffx > xtol))
      warning ("X axis differs from checkpoints. ",
               sprintf ("Maximum difference = %0.2g (%0.2g * XFACTOR)",
                        max (diffx), max (diffx) / hdr$xfactor))

  y <- y * hdr$yfactor

  new ("hyperSpec", spc = y, wavelength = wl)
}

.jdx.TABULAR.AFFN <- function (hdr, data, ...){
  data <- strsplit (data, "[[:blank:]]+")
  data <- unlist (data)
  spc <- read.txt.long  (textConnection (data),
                  cols = list (.wavelength="", spc=""),
                  header = FALSE, sep = ",")

  spc@wavelength <- spc@wavelength * hdr$xfactor
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
