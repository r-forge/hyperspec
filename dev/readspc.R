readspc <- function(filepath, filename)
{
	# Um innere Funktionen zu nutzen müssen diese vor ihrer ersten Nutzung
	# stehen. (Zumindest bis jetzt sieht es so aus)
	FTFLGS <- function(input)
	{
		if ( input == 0)
		{
			out <- "0x00h"
		}
		if ( input == 1)
		{
			out <- "0x01h"
		}
		if ( input %in% c(0, 1) == FALSE)
		{
			out <- input # in R keine einfache Funktion für dec2hex
		}
		out
	}
	FVERSN <- function(input)
	{
		if ( input %in% c(75, 76, 77))
		{
			if ( input == 75)
			{
				out <- 'newversionlsb' # new LSB 1st
			}
			if ( input == 76)
			{
				out <- 'newversionmsb' # new MSB 1st (different word order)
			}
			if ( input == 77)
			{
				out <- 'oldversion'
			}
		}
		else
		{
			out <- 'unknown'
		}
		out
	}
	FEXP <- function(input)
	{
		if (input == 128)
		{
			out <- 'float'
		}
		else
		{
			out <- input
		}
		out
	}
	FXTYPE <- function(input)
	{
		out <- vector("character", 2)
		if ( input == 0)
		{
		  out[1] = ''
		  out[2] = 'Arbitrary units'
		}
		if ( input == 1)
		{
			out[1] = 'cm-1'
			out[2] = 'Wavenumbers'
		}
		if ( input == 2)
		{
		  out[1] = 'microns'
		  out[2] = 'Micrometers'
		}
		if ( input == 3)
		{
		  out[1] = 'nano m'
		  out[2] = 'Nanometers'
		}
		if ( input == 4)
		{
		  out[1] = 's'
		  out[2] = 'Seconds'
		}
		if ( input == 5)
		{
		  out[1] = 'min'
		  out[2] = 'Minutes'
		}
		if ( input == 6)
		{
		  out[1] = 'Hz'
		  out[2] = 'Hertz'
		}
		if ( input == 7)
		{
		  out[1] = 'kHz'
		  out[2] = 'Kilohertz'
		}
		if ( input == 8)
		{
		  out[1] = 'MHz'
		  out[2] = 'Megahertz'
		}
		if ( input == 9)
		{
		  out[1] = 'M/z'
		  out[2] = 'Mass'
		}
		if ( input == 10)
		{
		  out[1] = 'ppm'
		  out[2] = 'Parts per million'
		}
		if ( input == 11)
		{
		  out[1] = 'd'
		  out[2] = 'Days'
		}
		if ( input == 12)
		{
		  out[1] = 'a'
		  out[2] = 'Years'
		}
		if ( input == 13)
		{
		  out[1] = 'cm-1'
		  out[2] = 'Raman Shift'
		}
		if ( input == 14)
		{
		  out[1] = 'eV'
		  out[2] = 'Electron Volts'
		}
		if ( input == 15)
		{
		  out[1] = '0x4Dh - FCATXT' # only when FVERSN = 0x4Dh, label is specified in FCATXT
		  out[2] = '0x4Dh - FCATXT'
		  warning('This axis value is not implemented yet');
		}
		if ( input == 16)
		{
		  out[1] = 'Diode No'
		  out[2] = 'Diode Number'
		}
		if ( input == 17)
		{
		  out[1] = 'Channel'
		  out[2] = 'Channel'
		}
		if ( input == 18)
		{
		  out[1] = 'Deg'
		  out[2] = 'Degrees'
		}
		if ( input == 19)
		{
		  out[1] = 'Deg F'
		  out[2] = 'Temperature (F)'
		}
		if ( input == 20)
		{
		  out[1] = 'Deg C'
		  out[2] = 'Temperature (C)'
		}
		if ( input == 21)
		{
		  out[1] = 'K'
		  out[2] = 'Temperature (K)'
		}
		if ( input == 22)
		{
		  out[1] = 'Points'
		  out[2] = 'Data points'
		}
		if ( input == 23)
		{
		  out[1] = 'ms'
		  out[2] = 'Milliseconds'
		}
		if ( input == 24)
		{
		  out[1] = 'micro s'
		  out[2] = 'microseconds'
		}
		if ( input == 25)
		{
		  out[1] = 'nano s'
		  out[2] = 'Nanoseconds'
		}
		if ( input == 26)
		{
		  out[1] = 'GHz'
		  out[2] = 'Gigahertz'
		}
		if ( input == 27)
		{
		  out[1] = 'cm'
		  out[2] = 'Centimeters'
		}
		if ( input == 28)
		{
		  out[1] = 'm'
		  out[2] = 'Meters'
		}
		if ( input == 29)
		{
		  out[1] = 'mm'
		  out[2] = 'Millimeters'
		}
		if ( input == 30)
		{
		  out[1] = 'h'
		  out[2] = 'Hours'
		}
		if ( input == 255)
		{
		  out[1] = ''
		  out[2] = 'Double interferogram'
		}
		if ( input %in% c(1:30, 255) == FALSE)
		{
		  out[1] = ''
		  out[2] = 'unknown'
		  warning('Unit not recognised')
		}
		out
	}
	FYTYPE <- function(input)
	{
		out <- vector("character", 2)

	if ( input == 0)
	{
      out[1] = ''
      out[2] = 'Arbitrary intensity'
	}
	if ( input == 1)
	{
      out[1] = ''
      out[2] = 'Interferogram'
	}
	if ( input == 2)
	{
      out[1] = ''
      out[2] = 'Absorbance'
	}
	if ( input == 3)
	{
      out[1] = ''
      out[2] = 'Kubelka-Munk'
	}
	if ( input == 4)
	{
      out[1] = 'counts'
      out[2] = 'Counts'
	}
	if ( input == 5)
	{
      out[1] = 'V'
      out[2] = 'Volts'
	}
	if ( input == 6)
	{
      out[1] = 'Deg'
      out[2] = 'Degrees'
	}
	if ( input == 7)
	{
      out[1] = 'mA'
      out[2] = 'Milliamps'
	}
	if ( input == 8)
	{
      out[1] = 'mm'
      out[2] = 'Millimetrs'
	}
	if ( input == 9)
	{
      out[1] = 'mV'
      out[2] = 'Millivolts'
	}
	if ( input == 10)
	{
      out[1] = 'log (1/R)'
      out[2] = 'LOG (1/R)'
	}
	if ( input == 11)
	{
      out[1] = '%'
      out[2] = 'Percent'
	}
	if ( input == 12)
	{
      out[1] = 'Int'
      out[2] = 'Intensity'
	}
	if ( input == 13)
	{
      out[1] = 'rel. Int'
      out[2] = 'Relative Intensity'
	}
	if ( input == 14)
	{
      out[1] = 'E'
      out[2] = 'Energy'
	}
	if ( input == 16)
	{
      out[1] = 'Decible'
      out[2] = 'Decible'
	}
	if ( input == 19)
	{
      out[1] = 'Deg F'
      out[2] = 'Temperature (F)'
	}
	if ( input == 20)
	{
      out[1] = 'Deg C'
      out[2] = 'Temperature (C)'
	}
	if ( input == 21)
	{
      out[1] = 'K'
      out[2] = 'Temperature (K)'
	}
	if ( input == 22)
	{
      out[1] = 'N'
      out[2] = 'Index of refraction'
	}
	if ( input == 23)
	{
      out[1] = 'k'
      out[2] = 'Extinction Coeff.'
	}
	if ( input == 24)
	{
      out[1] = 'real'
      out[2] = 'Real'
	}
	if ( input == 25)
	{
      out[1] = 'img'
      out[2] = 'Imaginary'
	}
	if ( input == 26)
	{
		out[1] = 'comp'
		out[2] = 'Complex'
	}
	if ( input == 27)
	{
		out[1] = 'cm'
		out[2] = 'Centimeters'
	}
	if ( input == 128) # All above 129 are assumed to have inverted peaks
	{
		out[1] = ''
		out[2] = 'Transmission'
	}
	if ( input == 129)
	{
		out[1] = ''
		out[2] = 'Reflectance'
	}
	if ( input == 130)
	{
		out[1] = ''
		out[2] = 'Arbitrary or single beam with valley peaks'
	}
	if ( input == 131)
	{
		out[1] = ''
		out[2] = 'Emission'
	}
	if ( input %in% c(1:27, 128:131) == FALSE)
	{
		out[1] = ''
		out[2] = 'unknown'
		warning('Unit not recognised')
	}
		out
	}
	origwd <- getwd()
	setwd(filepath)
	filepath <- getwd()
	filepath = paste(filepath, "/", filename, sep = "")
	filecon <- file(filepath)
	open(filecon, "rb")
	# Ausgabe: In Matlab eine Struktur, in R einfacherweise eine Liste
	output <- list()
	bytes <- readBin(filecon, "integer", 4, 1, signed = FALSE) # bytes = fread (fid, 4, 'uint8');
	if (FVERSN(bytes[2]) != 'newversionlsb')
	{
		warning('Wrong spc file format version.')
	}
	if (FEXP(bytes[4]) != 'float')
	{
		if (FEXP(bytes[4]) > 128)
		{
			warning('Erronous data type detected, setting conversion exponent to (value - 256)')
			comp <- FEXP(bytes[4])-256
			comp <- 2^comp / 2^32
		}
		else
		{
			comp = FEXP(bytes[4])
			comp = 2^comp / 2^32
		}
	}
	integer <- readBin(filecon, "integer", 2, 2, signed = FALSE)
	FNPTS <- integer[1]
	output$npoints <- FNPTS
	double <- readBin(filecon, "double", 2)
	output$x <- t(seq(double[1], double[2], (double[2]-double[1])/(integer[1]-1)))
	FNSUB <- readBin(filecon, "integer", 1, 2, signed = FALSE)
	if (FNSUB > 1)
	{
		stop('Subfiles not yet implemented')
	}
	# Zwischenräume
	temp <- readBin(filecon, "integer", 1, 2, signed = FALSE)
	# Hier können die Einheiten und Bezeichnungen der Achsen abgelesen werden,
	# relevant sind nur bytes 1 und 2
	bytes <- readBin(filecon, "integer", 4, 1, signed = FALSE)
	#  x, y, z axis units
	strings = FXTYPE(bytes[1])
	output$xunits = strings[1]
	output$xlabel = strings[2]

	strings = FYTYPE(bytes[2])
	output$yunits = strings[1]
	output$ylabel = strings[2]
	
	number <- readBin(filecon, "integer", 1, 4, signed = FALSE)
	# In Matlab ist es leicht aus dieser Zahl wieder Binärwerte zu gewinnen
	# und zu interpretieren. In R ist das meinem Wissensstand nach nicht einfach möglich
	# skip 476 bytes
	temp <- readBin(filecon, "integer", 476, 1, signed = FALSE) # fread (fid, 476, 'uint8');
	#    % skip SUBHDR
	temp <- readBin(filecon, "integer", 32, 1, signed = FALSE) # fread (fid, 32, 'uint8');
	#   % read following data points
	if (exists("comp") == FALSE)
	{
		output$y <- t(readBin(filecon, "numeric", output$npoints, 4))
	}
	else
	{
		output$y <- t(readBin(filecon, "integer", output$npoints, 4))
		output$y <- output$y * comp
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
