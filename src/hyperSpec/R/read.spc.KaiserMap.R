###-----------------------------------------------------------------------------
###
###  read.spc.KaiserMap
###
###

##' 
##' \code{read.spc.KaiserMap} imports sets of .spc files of Raman maps written
##' by Kaiser Optical Systems' Hologram software.  It may also serve as an
##' example how to write wrapper functions for \code{read.spc} to conveniently
##' import specialized sets of .spc files.
##' 
##' @export
##' @rdname read-spc
##' @param glob If \code{TRUE} the filename is interpreted as a wildcard
##'   containing file name pattern and expanded to all matching file names.
##' @param \dots All further arguments to \code{read.spc.KaiserMap} are handed
##'   over directly to \code{read.spc}.
read.spc.KaiserMap <- function (files, 
		keys.hdr2data = FALSE, keys.hdr2log = FALSE,
		keys.log2data = NULL, keys.log2log = FALSE, 
		glob = TRUE, ...) {
    
	if (glob)
		files <- Sys.glob (files)

   if (length (files) == 0){
     warning ("No files found.")
     return (new ("hyperSpec"))
   }

	keys.log2data <- c ('Stage_X_Position','Stage_Y_Position','Stage_Z_Position', keys.log2data)
	
	f <- files [1]
	
	spc <- read.spc (f,
                    keys.log2data = keys.log2data,
                    keys.hdr2data = keys.hdr2data,
                    keys.hdr2log = keys.hdr2log, 
                    keys.log2log = keys.log2log, no.object = TRUE, ...)

   data <- spc$data [rep (1L, length (files)),, drop = FALSE]
	
	spc$spc  <- spc$spc  [rep (1L, length (files)), , drop = FALSE]
	
	for (f in seq_along (files)){
		tmp <- read.spc (files [f], keys.log2data = keys.log2data,
                       keys.hdr2data = keys.hdr2data,
                       no.object = TRUE, ...)
      
      data [f, ] <- tmp$data 
		spc$spc  [f, ] <- tmp$spc
	}

   data <- data [, ! colnames (data) %in% c ("z", "z.end"), drop = FALSE]
   colnames (data) <- gsub ("Stage_(.)_Position", "\\L\\1", colnames (data), perl = TRUE)


   for (cln in c ("x", "y", "z"))
       data [[cln]] <- as.numeric (data [[cln]])
   
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

