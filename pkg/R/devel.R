# development file
# 
# Author: cb
###############################################################################

file = "~/Uni/Projekte/Alois-cartilage/080606/080606c.txt"
file = "~/Uni/Projekte/Spikes/static-high.txt"

system.time (spc2 <- scan.txt.Renishaw (file, "ts"))
system.time (spc <- read.txt.Renishaw (file, "ts"))
###-----------------------------------------------------------------------------
###
###  read.txt.Renishaw - import Raman measurements from Renishaw .txt file
###  
###  Renishaw .wxd files are converted to .txt ASCII files by their batch converter. 
###  Format:
###  (y x | t | z) wl int
###  
scan.txt.Renishaw <- function (file = stop ("filename is required"), data = "xyspc", 
		nlines = 0, nspc = NULL, ...){
	cols <- c (
			switch (data,
					spc = NULL,   
					xyspc = c (y = expression ("/" (y, mu * m)), 
							   x = expression ("/" (x, mu * m))), 
					zspc = ,
					depth = c(z = expression ("/" (z, mu * m))),
					ts = 	c(t= "t / s"),
					stop ("unknown format for Renishaw .txt files.")
			),
			.wavelength = expression (tilde(nu) / cm^-1) ,
			spc = "I / a.u.")	

	first <- scan(file, nlines = 1, quiet = TRUE)
	ncol <- length (first)
	
	if (ncol != length (cols))
		stop (paste ("File has", ncol, "columns, while 'cols' gives", length (cols)))
	
	
	
	file <- file (file, "r")
	on.exit(close(file))
		
	fbuf <- matrix (scan (file, quiet = TRUE), ncol = ncol, byrow = TRUE)
	
	wl <- unique (fbuf[, ncol - 1])
	
	## if the file is to be read in chunks
	## try to find out how many lines it has 
	if (is.null (nspc))
		if (nlines > 0){ 
			nspc <- wc (file, "lines")
			if (is.null (nspc))
				stop ("failed guessing nspc.")
			else
				nspc <- nspc[1]
		} else {
			nspc <- nrow (fbuf) / length (wl)
		}
	
	data <- matrix (NA, ncol = ncol - 2, nrow = nspc)
	colnames (data) <- head (names (cols), -2) 
	pos.data <- 0

	spc <- numeric (nspc * length (wl))
	pos.spc <- 0

	while (length (fbuf > 0)){
		spc [pos.spc + seq_len (nrow (fbuf))] <- fbuf [, ncol]
		pos.spc <- pos.spc + nrow (fbuf)
		
		dummy <- fbuf [fbuf[, ncol - 1] == wl [1], seq_len (ncol - 2), drop = FALSE]
		
		data [pos.data + seq_len (nrow (dummy)), ] <- dummy
		pos.data <- pos.data + nrow (dummy)
		
		fbuf <- matrix (scan (file, quiet = TRUE), ncol = ncol, byrow = TRUE)

		if (length (fbuf > 0) & ! all(unique (fbuf[, ncol - 1]) %in% wl))
			stop ("Wavelengths do not correspond to that of the other chunks. Is the size of the first chunk large enough to cover a complete spectrum?")
	}
	
	spc <- matrix (spc, ncol = length (wl), nrow = nspc, byrow = TRUE)
	
	new ("hyperSpec", spc = spc, data = as.data.frame (data), wavelength = wl, label = cols,
			log = list (
					short = "scan.txt.Renishaw",
					long = list (file = file, cols = I (cols), ...)
			)
	)
}

file = "/data/lgfile/*.txt"
flags = "lines"


#' wc (word count) 
#' @param file the file name or pattern
#' @param flags the parameters to count, as character vector of the long form. 
#' @returnType 
#' @return data.frame with the counts and file names
#' @author cb
#' @warning stops with an error if wc does not exist
#' @export
wc <- function (file, flags = c("lines", "words", "bytes")){
	if (length (system ("wc --help", internal = TRUE)) == 0)
		return (NULL)
	
	wc <- paste ("wc", paste ("--", flags, sep = "", collapse = ", "), file)
	wc <- read.table(pipe (wc))
	colnames (wc) <- c(flags, "file")
	wc
} 

wc (file, "lines")$lines[1]


file <- "~/Uni/Projekte/InflammatoryBowelDiseases/RCU/SpecchioElia/detail/detail-high.txt"
r <- read.txt.Renishaw (file)
apropos ("string")

buf <- readLines (file, n = 1)
dummy <-  (buf)


file <- "~/Uni/Projekte/InflammatoryBowelDiseases/RCU/SpecchioElia/detail/detail-high.txt"

system.time (scan (file))
system.time (scan (file, nlines = 10299400/2))
system.time (scan (file, nlines = 10299400/2, skip = 10299400/2))
system.time (dummy <- matrix (scan (file, quiet = TRUE), ncol = ncol, byrow = TRUE))
file.info (file)
??wc

dummy <- matrix (nrow = 10299400, ncol = 4)
		
f <- file(file, "r")
scan (f, nlines = 2)
scan (f, nlines = 2)


close(f)




file = "/data/lgfile/lg2.txt"
#system.time (dummy <- matrix (scan (file, quiet = TRUE), ncol = ncol, byrow = TRUE))
#system.time (dummy <- scan (file, quiet = TRUE))
system.time (dummy <- readLines (file))


system.time (dummy <- as.matrix (read.table (file)))

function (file, header = FALSE, sep = "", quote = "\"'", dec = ".", 
		row.names, col.names, as.is = !stringsAsFactors, na.strings = "NA", 
		colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, 
		fill = !blank.lines.skip, strip.white = FALSE, blank.lines.skip = TRUE, 
		comment.char = "#", allowEscapes = FALSE, flush = FALSE, 
		stringsAsFactors = default.stringsAsFactors(), encoding = "unknown") 
{
	if (is.character(file)) {
		file <- file(file, "r")
		on.exit(close(file))
	}
	if (!inherits(file, "connection")) 
		stop("'file' must be a character string or connection")
	if (!isOpen(file, "r")) {
		open(file, "r")
		on.exit(close(file))
	}
	if (skip > 0) 
		readLines(file, skip)
	nlines <- n0lines <- if (nrows < 0) 
				5
			else min(5, (header + nrows))
	lines <- .Internal(readTableHead(file, nlines, comment.char, 
					blank.lines.skip, quote, sep))
	nlines <- length(lines)
	if (!nlines) {
		if (missing(col.names)) 
			stop("no lines available in input")
		rlabp <- FALSE
		cols <- length(col.names)
	}
	else {
		if (all(!nzchar(lines))) 
			stop("empty beginning of file")
		if (nlines < n0lines && file == 0) {
			pushBack(c(lines, lines, ""), file)
			on.exit(.Internal(clearPushBack(stdin())))
		}
		else pushBack(c(lines, lines), file)
		first <- scan(file, what = "", sep = sep, quote = quote, 
				nlines = 1, quiet = TRUE, skip = 0, strip.white = TRUE, 
				blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
				allowEscapes = allowEscapes, encoding = encoding)
		col1 <- if (missing(col.names)) 
					length(first)
				else length(col.names)
		col <- numeric(nlines - 1)
		if (nlines > 1) 
			for (i in seq_along(col)) col[i] <- length(scan(file, 
								what = "", sep = sep, quote = quote, nlines = 1, 
								quiet = TRUE, skip = 0, strip.white = strip.white, 
								blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
								allowEscapes = allowEscapes))
		cols <- max(col1, col)
		rlabp <- (cols - col1) == 1
		if (rlabp && missing(header)) 
			header <- TRUE
		if (!header) 
			rlabp <- FALSE
		if (header) {
			readLines(file, 1)
			if (missing(col.names)) 
				col.names <- first
			else if (length(first) != length(col.names)) 
				warning("header and 'col.names' are of different lengths")
		}
		else if (missing(col.names)) 
			col.names <- paste("V", 1:cols, sep = "")
		if (length(col.names) + rlabp < cols) 
			stop("more columns than column names")
		if (fill && length(col.names) > cols) 
			cols <- length(col.names)
		if (!fill && cols > 0 && length(col.names) > cols) 
			stop("more column names than columns")
		if (cols == 0) 
			stop("first five rows are empty: giving up")
	}
	if (check.names) 
		col.names <- make.names(col.names, unique = TRUE)
	if (rlabp) 
		col.names <- c("row.names", col.names)
	nmColClasses <- names(colClasses)
	if (length(colClasses) < cols) 
		if (is.null(nmColClasses)) {
			colClasses <- rep(colClasses, length.out = cols)
		}
		else {
			tmp <- rep(NA_character_, length.out = cols)
			names(tmp) <- col.names
			i <- match(nmColClasses, col.names, 0)
			if (any(i <= 0)) 
				warning("not all columns named in 'colClasses' exist")
			tmp[i[i > 0]] <- colClasses
			colClasses <- tmp
		}
	what <- rep.int(list(""), cols)
	names(what) <- col.names
	colClasses[colClasses %in% c("real", "double")] <- "numeric"
	known <- colClasses %in% c("logical", "integer", "numeric", 
			"complex", "character")
	what[known] <- sapply(colClasses[known], do.call, list(0))
	what[colClasses %in% "NULL"] <- list(NULL)
	keep <- !sapply(what, is.null)
	data <- scan(file = file, what = what, sep = sep, quote = quote, 
			dec = dec, nmax = nrows, skip = 0, na.strings = na.strings, 
			quiet = TRUE, fill = fill, strip.white = strip.white, 
			blank.lines.skip = blank.lines.skip, multi.line = FALSE, 
			comment.char = comment.char, allowEscapes = allowEscapes, 
			flush = flush, encoding = encoding)
	nlines <- length(data[[which(keep)[1]]])
	if (cols != length(data)) {
		warning("cols = ", cols, " != length(data) = ", length(data), 
				domain = NA)
		cols <- length(data)
	}
	if (is.logical(as.is)) {
		as.is <- rep(as.is, length.out = cols)
	}
	else if (is.numeric(as.is)) {
		if (any(as.is < 1 | as.is > cols)) 
			stop("invalid numeric 'as.is' expression")
		i <- rep.int(FALSE, cols)
		i[as.is] <- TRUE
		as.is <- i
	}
	else if (is.character(as.is)) {
		i <- match(as.is, col.names, 0)
		if (any(i <= 0)) 
			warning("not all columns named in 'as.is' exist")
		i <- i[i > 0]
		as.is <- rep.int(FALSE, cols)
		as.is[i] <- TRUE
	}
	else if (length(as.is) != cols) 
		stop(gettextf("'as.is' has the wrong length %d  != cols = %d", 
						length(as.is), cols), domain = NA)
	do <- keep & !known
	if (rlabp) 
		do[1] <- FALSE
	for (i in (1:cols)[do]) {
		data[[i]] <- if (is.na(colClasses[i])) 
					type.convert(data[[i]], as.is = as.is[i], dec = dec, 
							na.strings = character(0))
				else if (colClasses[i] == "factor") 
					as.factor(data[[i]])
				else if (colClasses[i] == "Date") 
					as.Date(data[[i]])
				else if (colClasses[i] == "POSIXct") 
					as.POSIXct(data[[i]])
				else methods::as(data[[i]], colClasses[i])
	}
	compactRN <- TRUE
	if (missing(row.names)) {
		if (rlabp) {
			row.names <- data[[1]]
			data <- data[-1]
			keep <- keep[-1]
			compactRN <- FALSE
		}
		else row.names <- .set_row_names(as.integer(nlines))
	}
	else if (is.null(row.names)) {
		row.names <- .set_row_names(as.integer(nlines))
	}
	else if (is.character(row.names)) {
		compactRN <- FALSE
		if (length(row.names) == 1) {
			rowvar <- (1:cols)[match(col.names, row.names, 0) == 
							1]
			row.names <- data[[rowvar]]
			data <- data[-rowvar]
			keep <- keep[-rowvar]
		}
	}
	else if (is.numeric(row.names) && length(row.names) == 1) {
		compactRN <- FALSE
		rlabp <- row.names
		row.names <- data[[rlabp]]
		data <- data[-rlabp]
		keep <- keep[-rlabp]
	}
	else stop("invalid 'row.names' specification")
	data <- data[keep]
	if (is.object(row.names) || !(is.integer(row.names))) 
		row.names <- as.character(row.names)
	if (!compactRN) {
		if (length(row.names) != nlines) 
			stop("invalid 'row.names' length")
		if (any(duplicated(row.names))) 
			stop("duplicate 'row.names' are not allowed")
		if (any(is.na(row.names))) 
			stop("missing values in 'row.names' are not allowed")
	}
	class(data) <- "data.frame"
	attr(data, "row.names") <- row.names
	data
}