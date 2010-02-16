###-----------------------------------------------------------------------------
###
###  read.spc.KaiserMap
###
###

read.spc.KaiserMap <- function (files, 
		keys.hdr2data = FALSE, keys.hdr2log = TRUE,
		keys.log2data = NULL, keys.log2log = TRUE, 
		glob = TRUE, ...) {
    
	if (glob)
		files <- Sys.glob (files)

   if (length (files) == 0){
     warning ("No files found.")
     return (new ("hyperSpec"))
   }

	keys.log2data <- c ('Stage_X_Position','Stage_Y_Position','Stage_Z_Position', keys.log2data)
	
	data <- data.frame (x = rep (NA, length (files)),
			y = rep (NA, length (files)),
			z = rep (NA, length (files)))
	
	f <- files [1]
	
	spc <- read.spc (f, keys.log2data = keys.log2data, keys.log2log = keys.log2log, no.object = TRUE, ...)

	data [1, 'x'] <- factor2num (spc$data$Stage_X_Position)
	data [1, 'y'] <- factor2num (spc$data$Stage_Y_Position)
	data [1, 'z'] <- factor2num (spc$data$Stage_Z_Position)
	
	spc$spc  <- spc$spc  [rep (1, length (files)), , drop = FALSE]
	
	for (f in seq_along (files)){
		tmp <- read.spc (files [f], keys.log2data = keys.log2data, keys.log2log = keys.log2log,
				no.object = TRUE, ...)
		
		data [f, 'x'] <- factor2num (tmp$data$Stage_X_Position)
		data [f, 'y'] <- factor2num (tmp$data$Stage_Y_Position)
		data [f, 'z'] <- factor2num (tmp$data$Stage_Z_Position)
		
		spc$spc  [f, ] <- tmp$spc
	}
	
	log <- list (short = "read.spc.KaiserMap",
					long = list (call = match.call (),
							last.header = tmp$log$long$header,
							last.log = tmp$log$long$log))

   data$file <- files
   
	new ("hyperSpec", wavelength = spc$wavelength, spc = spc$spc, data = data, 
			label = tmp$label,
			log = log)
}

