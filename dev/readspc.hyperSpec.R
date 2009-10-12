readspc <- function(filepath, filename) {
  ## Define constants and functions

  ## File Date Type Flags
  ## ORed to determine data type
  FTFLGS <- c(TSPREC =   1,
              TCGRAM =   2,
              TMULTI =   4,
              TRANDM =   8,
              TORDRD =  16,
              TALABS =  32,
              TXYXYS =  64,
              TXVALS = 128
              )
  storage.mode (FTFLGS) <- "raw"
  
  ## SPC File Format  Version (magic number)
  FVERSN <- c (NEW.LSB = 75, NEW.MSB = 76, OLD = 77)
  storage.mode (FVERSN) <- "raw"
  
  ## Instrumental Experiment Technique
  FEXPER <- c (SPCGEN  =  0,
               SPCGC   =  1,
               SPCCGM  =  2,
               SPCHPLC =  3, 
               SPCFTIR =  4,
               SPCNIR  =  5,
               SPCUV   =  6,
               ## 7 is undefined
               SPCXRY  =  8,
               SPCMS   =  9,
               SPCNMR  = 10,
               SPCRMN  = 11,
               SPCFLR  = 12,
               SPCATM  = 13,
               SPCDAD  = 14
               )
  storage.mode (FEXPER) <- "raw"
  
  FEXP <- c(FLOAT = 128)
  storage.mode (FEXP) <- "raw"

  FXTYPE <- c (expression (`/` (x, "a. u.")),                      #0
               expression (`/` (tilde (nu), cm^-1)),
               expression (`/` (lambda, (mu * m))),
               expression (`/` (lambda, nm)),
               expression (`/` (t, s)),
               expression (`/` (t, min)),
               expression (`/` (f, Hz)),
               expression (`/` (f, kHz)),
               expression (`/` (f, MHz)),
               expression (`/` (frac (m, z), frac (u, e))),        
               expression (`/` (delta, ppm)),                      # 10
               expression (`/` (t, d)),
               expression (`/` (t, a)),
               expression (`/` (delta*tilde (nu), cm^-1)),
               expression (`/` (E, eV)),
               NA, # old version file uses label in gcatxt
               'Diode No',
               'Channel',
               expression (`/` (x, degree)),
               expression (`/` (T, degree*F)),
               expression (`/` (T, degree*C)),                     # 20
               expression (`/` (T, K)),
               'Data Point',
               expression (`/` (t, ms)),
               expression (`/` (t, micro*s)),
               expression (`/` (t, ns)),
               expression (`/` (f, GHz)),
               expression (`/` (lambda, cm)),
               expression (`/` (lambda, m)),
               expression (`/` (lambda, mm)),
               expression (`/` (t, h))                             # 30
               )
  
 xlab <- function (x) {
    if (x <= length (FXTYPE) + 1)
      FXTYPE [x + 1]
    else
      NA
  }

  FYTYPE <- c (expression (`/` (I, "a. u.")),                      #0
               expression (`/` (I, "a. u.")),
               'A',
               expression (frac ((1 - R)^2, 2 * R)),
               'Counts',
               expression (`/` (U, V)),
               expression (`/` (y, degree)),
               expression (`/` (I, mA)),
               expression (`/` (l, mm)),
               expression (`/` (U, mV)),
               expression (-log (R)),                             # 10
               expression (`/` (y, '%')),
               expression (`/` (I, 'a. u.')),        
               expression (I / I[0]),                             
               expression (`/` (E, J)),
               NA, # old version file uses label in gcatxt
               expression (`/` (G, dB)),
               NA, # old version file uses label in gcatxt
               NA, # old version file uses label in gcatxt
               expression (`/` (T, degree*F)),
               expression (`/` (T, degree*C)),                    # 20
               'n',
               'epsilon',
               expression (Re (y)),
               expression (Im (y)),
               'T',
               'R',
               expression (`/` (I, 'a. u.')),
               expression (`/` (I, 'a. u.'))
               )
  
ylab <- function(x){
  if (x <= 26)
    FYTYPE [x + 1]
  else if (x %in% 128 : 131)
    FYTYPE [x - 101]
  else
    NA
}

  FDATE.MIN <- c(as.raw (c(0, 0, 0, 63)))
  FDATE.H   <- c(as.raw (c(0, 0, 7, 192)))
  FDATE.D   <- c(as.raw (c(0, 0, 248, 0)))
  FDATE.M   <- c(as.raw (c(0, 15, 0, 0)))
  FDATE.Y   <- c(as.raw (c(255, 240, 0, 0)))

  .readheader <- function (header) {
    if (header [2] != FVERSN ['NEW.LSB'])
      stop ('Wrong spc file format version. For now, only spc files of new version with LSB are supported.\nPlease contact the package maintainer (see package?hyperSpec).') 

    fdate <- readBin(header [ 33 :  36], "integer", 1, 4, signed = FALSE)
    
    list (ftflgs   = header [1],
          fexper   = readBin   (header [        3], "integer", 1, 1, signed = TRUE),
          fexp     = readBin   (header [        4], "integer", 1, 1, signed = TRUE),
          fnpts    = readBin   (header [  5 :   8], "integer", 1, 4, signed = FALSE),
          ffirst   = readBin   (header [  9 :  16], "double", 1),
          flast    = readBin   (header [ 17 :  24], "double", 1),
          fnsub    = readBin   (header [ 25 :  28], "integer", 1, 4, signed = FALSE),
          fxtype   = xlab (readBin(header [    29], "integer", 1, 1, signed = FALSE)),
          fytype   = ylab (readBin(header [    30], "integer", 1, 1, signed = FALSE)),
          fztype   = xlab (readBin(header [    31], "integer", 1, 1, signed = FALSE)),
          fpost    = readBin   (header [       32], "integer", 1, 1, signed = TRUE),
          fdate    = ISOdate (year  = fdate %/% 1048560,
            month = fdate %/% 65536 %%  16,
            day   = fdate %/% 2048 %% 32,
            hour  = fdate %/% 64 %% 32,
            min   = fdate %% 64),
          fres     = paste (rawToChar (header [ 37 :  45], multiple = TRUE), collapse = ""),
          fsource  = paste (rawToChar (header [ 46 :  54], multiple = TRUE), collapse = ""),
          fpeakpt  = readBin   (header [ 55 :  56], "integer", 1, 2, signed = FALSE),
          fspare   = readBin   (header [ 57 :  88], "numeric", 8, 4),
          fcmnt    = paste (rawToChar (header [ 89 : 218], multiple = TRUE), collapse = ""),
          fcatxt   = paste (rawToChar (header [219 : 248], multiple = TRUE), collapse = ""),
          flogoff  = readBin   (header [249 : 252], "integer", 1, 4, signed = FALSE),
          fmods    = readBin   (header [253 : 256], "integer", 1, 4, signed = FALSE),
          fprocs   = readBin   (header [      257], "integer", 1, 1, signed = TRUE),
          flevel   = readBin   (header [      258], "integer", 1, 1, signed = TRUE),
          fsampin  = readBin   (header [259 : 260], "integer", 1, 2, signed = FALSE),
          ffactor  = readBin   (header [261 : 264], "numeric", 1, 4),
          fmethod  = paste (rawToChar (header [265 : 312], multiple = TRUE), collapse = ""),
          fzinc    = readBin   (header [313 : 316], "numeric", 1, 4, signed = FALSE),
          fwplanes = readBin   (header [317 : 320], "integer", 1, 4, signed = FALSE),
          fwinc    = readBin   (header [321 : 324], "numeric", 1, 4),
          fwtype   = readBin   (header [      325], "integer", 1, 1, signed = TRUE)
          ## 187 bytes reserved
          )

  }

  .readsubheader <- function (subheader) {
    list (subflgs   =          subheader [      1],
          subexp    = readBin (subheader [      2], "integer", 1, 1, signed = FALSE),
          subindx   = readBin (subheader [ 3 :  4], "integer", 1, 2, signed = FALSE),
          subtime   = readBin (subheader [ 5 :  8], "numeric", 1, 4),
          subnext   = readBin (subheader [ 9 : 12], "numeric", 1, 4),
          subtime   = readBin (subheader [13 : 16], "numeric", 1, 4),
          subnois   = readBin (subheader [17 : 20], "integer", 1, 4, signed = FALSE),
          subnpts   = readBin (subheader [21 : 24], "integer", 1, 4, signed = FALSE),
          subwlevel = readBin (subheader [25 : 28], "numeric", 1, 4)
          )
  }


###########################################################################################################

  f <- readBin (filepath, "raw", file.info (filepath)$size, 1)

  header <- .readheader (f [1 : 512])

  if (header$fnsub > 1)
    stop('Subfiles not yet implemented')
  
  if (! header$ftflgs & FTFLGS ['TXVALS'])
    wavelengths <- diff (header$range) / (header$fnpts - 1) * seq (0, header$fnpts - 1) +
      header$range [1]

  ## Subfiles
  isub <- 0 
  fpos <- 512
  
  subheader <- .readsubheader (f [fpos + 1 : 32])
  fpos <- fpos + 32

  if ((header$ftflgs & FTFLGS ['TXVALS']) && !isub)
    
    
    
  
	number <- readBin(filecon, "integer", 1, 4, signed = FALSE)
	# In Matlab ist es leicht aus dieser Zahl wieder Binärwerte zu gewinnen
	# und zu interpretieren. In R ist das meinem Wissensstand nach nicht einfach möglich
	# skip 476 bytes
	temp <- readBin(filecon, "integer", 476, 1, signed = FALSE) # fread (fid, 476, 'uint8');
	#    % skip SUBHDR
	temp <- readBin(filecon, "integer", 32, 1, signed = FALSE) # fread (fid, 32, 'uint8');
	#   % read following data points
	if (exponent == -128) {
          output$y <- t(readBin(filecon, "numeric", output$npoints, 4))
	} else 	{
          output$y <- t(readBin(filecon, "integer", output$npoints, 4))
          output$y <- output$y * 2^(comp - 32) ## todo 16 bit files
	}
	# Skip
	temp <- readBin(filecon, "integer", 64, 1, signed = FALSE)
	# Retrieve text comments
	textpos <- c('Stage_X_Position=', 'Stage_Y_Position=', 'Stage_Z_Position=', 'AutoFocusUsed')
	parameters <- vector("list", length(textpos) - 1)
	names(parameters) <- substr(textpos, 1, nchar(textpos) - 1)[1:3]
	fparameters <- names(parameters)
	text <- readBin(filecon, "raw", 2000, 1, signed = FALSE)
	text <- rawToChar(text, multiple = TRUE)
	ind1 <- which(text == "\r")
	ind1 <- ind1 - 1
	ind2 <- which(text == "\n")
	ind2 <- ind2 + 1
	ind2 <- c(1, ind2)
	output$parameters <- list()
	if ( length(ind1) > 0)
	{
	for ( i in 1 : (length(ind1)-1))
	{
		output$parameters[[i]] <- paste(text[ind2[i]:ind1[i]], collapse = "")
	}
	output$position <- vector("numeric", 3)
	for ( i in 1 : (length(textpos)-1))
	{
		pos <- grep(fparameters[i], output$parameters)
		if ( length(pos > 0))
		{
			ind <- which(unlist(strsplit(output$parameters[[pos]], NULL)) == "=")
			output$position[i] <- as.numeric(substr(output$parameters[[pos]], (ind + 1), nchar(output$parameters[[pos]])))
		}
	}
	output$Exposure <- "NA"
	output$Accu <- "NA"
	if ( length(grep("Exposure_Length", output$parameters)) > 0)
	{
		pos <- grep("Exposure_Length", output$parameters)
		ind <- which(unlist(strsplit(output$parameters[[pos]], NULL)) == "=")
		output$Exposure <- as.numeric(substr(output$parameters[[pos]], (ind + 1), nchar(output$parameters[[pos]])))
	}
	if ( length(grep("Accumulations", output$parameters)) > 0)
	{
		pos <- grep("Accumulations", output$parameters)
		ind <- which(unlist(strsplit(output$parameters[[pos]], NULL)) == "=")
		output$Accu <- as.numeric(substr(output$parameters[[pos]], (ind + 1), nchar(output$parameters[[pos]])))
	}
	}
	close(filecon)
	setwd(origwd)
	output
}
