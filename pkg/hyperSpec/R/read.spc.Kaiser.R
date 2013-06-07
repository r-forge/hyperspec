##' Import functions for Kaiser Optical Systems .spc files
##' 
##' \code{read.spc.Kaiser} imports sets of .spc files written by Kaiser Optical Systems' Hologram
##' software.  It may also serve as an example how to write wrapper functions for \code{read.spc} to
##' conveniently import specialized sets of .spc files.
##' 
##' @title read Kaiser .spc files
##' @export
##' @rdname read-spc-Kaiser
##' @param files If \code{glob = TRUE}, \code{filename} can contain wildcards.
##'   Thus all files matching the name pattern in \code{filename} can be
##'   specified.
##' @param glob If \code{TRUE} the filename is interpreted as a wildcard
##'   containing file name pattern and expanded to all matching file names.
##' @param keys.log2data,\dots All further arguments are handed over directly to \code{\link{read.spc}}.
##' @return hyperSpec 
##' @examples
##' ## for examples, please see `vignette ("fileio")`.

read.spc.Kaiser <- function (files, ..., glob = TRUE) {
    
	if (glob)
		files <- Sys.glob (files)

   if (length (files) == 0){
     warning ("No files found.")
     return (new ("hyperSpec"))
   }
	
	f <- files [1]
	
	spc <- read.spc (f, no.object = TRUE, ...)

   data <- spc$data [rep (1L, length (files)),, drop = FALSE]
	
	spc$spc  <- spc$spc  [rep (1L, length (files)), , drop = FALSE]
	
	for (f in seq_along (files)){
		tmp <- read.spc (files [f], no.object = TRUE, ...)
      
      data [f, ] <- tmp$data 
		spc$spc  [f, ] <- tmp$spc
	}

   
  if (hy.getOption ("log")){
    warning ("The logbook is deprecated and will soon be removed.")
    log <- list (short = "read.spc.KaiserMap",
                 long = list (call = match.call (),
                   last.header = tmp$log$long$header,
                   last.log = tmp$log$long$log))
  } else {
    log <- NULL
  }
   
   data$file <- files
   
	new ("hyperSpec", wavelength = spc$wavelength, spc = spc$spc, data = data, 
			labels = tmp$label,
			log = log)
}

##' \code{read.spc.KaiserMap} is a wrapper for \code{read.spc.Kaiser} with predefined \code{log2data}
##' to fetch the stage position for each file.
##' @rdname read-spc-Kaiser
##' @export
read.spc.KaiserMap <- function (files, keys.log2data = NULL, ...) {
  keys.log2data <- c ('Stage_X_Position','Stage_Y_Position','Stage_Z_Position', keys.log2data)

  spc <- read.spc.Kaiser (files, keys.log2data = keys.log2data, ...)
  
  spc@data <- spc@data [, ! colnames (spc@data) %in% c ("z", "z.end"), drop = FALSE]

  colnames (spc@data) <- gsub ("Stage_(.)_Position", "\\L\\1", colnames (spc@data), perl = TRUE)
  for (cln in c ("x", "y", "z"))
      spc@data [[cln]] <- as.numeric (spc@data [[cln]])

  spc@label$x <- expression (`/` (x, micro * m))
  spc@label$y <- expression (`/` (y, micro * m))
  spc@label$z <- expression (`/` (z, micro * m))
  spc@label$z.end <- NULL
  
  spc
}

##' \code{read.spc.KaiserLowHigh} is a wrapper for \code{read.spc.Kaiser} for raw data that is saved
##' in separate files for low and high wavenumber range.  The wavelength axis holds the pixel
##' numbers, which repeat for low and high wavenumber ranges. This is how the wavelength calibration
##' in \code{\link{wlcal.Kaiser}} expects it.
##' 
##' @rdname read-spc-Kaiser
##' @param type what kind of measurement was done? If \code{"map"}, \code{read.spc.KaiserMap} is used
##' instead of \code{read.spc.Kaiser}.
##' @export
read.spc.KaiserLowHigh <- function (files = stop ("file names needed"),
                                    type = c ("single", "map"),
                                    ..., glob = TRUE) {

	if (glob)
       files <- Sys.glob (files)
   
   files <- matrix (files, nrow = 2)

   type <- match.arg (type)
   switch (type,
           single = cbind (read.spc.Kaiser    (files [1,], ..., glob = FALSE),
                           read.spc.Kaiser    (files [2,], ..., glob = FALSE)),
           map    = cbind (read.spc.KaiserMap (files [1,], ..., glob = FALSE),
                           read.spc.KaiserMap (files [2,], ..., glob = FALSE))
           )

}
##' Wavelength and Wavenumber calibration for Kaiser
##'
##' This function performs two calibration steps for the spectral abscissa:
##'
##' \bold{wavelength} calibration converts the CCD's pixel number into wavelengths (in nm). This uses
##' the calibration settings read from sections "Chan1 Plex1 Parameters" and "Chan1 Plex2 Parameters"
##' of the .wcl file. If \code{wcl} is not given, this step is skipped (i.e. the wavelength is
##' supposed to hold already a proper wavelength).
##'
##' \bold{wavenumber} calibration converts the wavelengths into Raman shift relative
##' wavenumbers. This needs the exact wavelength of the excitation laser (in nm). Alternatively, an
##' .spc file written by Kaiser Hologram can be given. These files record the the laser wavelength in
##' log entry "Laser_Wavelength". If \code{laser} is missing, this step is skipped and wavelengths
##' are returned.
##' 
##' @title Wavelength and Wavenumber calibration for Kaiser Raman
##' @param spc hyperSpec object with wavelength in CCD pixels
##' @param wcl connection to Kaiser wavelength calibration (.wcl) file, see details. 
##' @param laser exact laser wavelength or connection to a Kaiser Hologram .spc file 
##' 
##' @param range which spectral ranges does the hyperSpec object contain?
##' @return wavelength-calibrated hyperSpec object 
##' @author Claudia Beleites
##' @seealso \code{\link{read.spc.Kaiser}} for importing Kaiser .spc files
##'
##' \code{\link{wl2relwn}} for wavelength to Raman shift (relative wavenumber) conversion.
##' @export 
wlcal.Kaiser <- function (spc, wcl, laser, range = c ("both", "low", "high")){

  if (! missing (wcl)){
      if (! is.list (wcl)){}
    spc <- .wlcal.Kaiser (spc, read.ini (wcl), range = match.arg (range))
    }
  
  if (! missing (laser)){

    if (! is.numeric (laser)) {
        laser <- read.spc (laser, keys.log2data = "Laser_Wavelength")
        laser <- as.numeric (laser$Laser_Wavelength)
      }
    
    spc <- wl2relwn (spc, laser)
  }

  validObject (spc)
  
  spc
}

.wlcal.Kaiser <- function (spc, wcl, range) {
  px <- spc@wavelength
  
  cal.low  <- unlist (wcl$Chan1.Plex1.Parameters)
  if (is.null (cal.low))
      stop ("Section 'Chan1 Plex1 Parameters' not found in wavelength calibration file.")
  
  cal.high <- unlist (wcl$Chan1.Plex2.Parameters)
  if (is.null (cal.high))
      stop ("Section 'Chan1 Plex2 Parameters' not found in wavelength calibration file.")

  switch (range,
          low = {
            ilow <- seq_along (px)
            ihigh <- FALSE
          },
          high = {
            ilow <- FALSE
            ihigh <- seq_along (px)
          },
          both = {
            ilow <- seq_len (which (diff (px) < 0))
            ihigh <- seq_along (px) [- ilow]
          }
          )

  wl.low  <- vanderMonde (px [ilow],  order = length (cal.low)  - 1) %*% cal.low
  wl.high <- vanderMonde (px [ihigh], order = length (cal.high) - 1) %*% cal.high

  spc@wavelength <- c (wl.low, wl.high)
  spc@label$.wavelength <- expression (lambda / nm)

  spc
}

