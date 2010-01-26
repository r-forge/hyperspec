## wish list: slice_map
## wish list: parse arg of form x..y = z => x = list (y = z)
# TODO: write tests
# TODO: check docs
# TODO: check .is.hy / validObject
# TODO: make consistent user/short/long/date



### ****************************************************************************
###
###  read & write hyperSpec objects
###
### ****************************************************************************




### ****************************************************************************
###
###  spectra-related functions
###
### ****************************************************************************




### ****************************************************************************
###
###  some more convenient functions, not operating on hyperSpec objects
###
### ****************************************************************************

###-----------------------------------------------------------------------------
###
###  vec2array -- vector index to array index conversion
###
###

vec2array <- function (ivec, dim) {
  ndim <- length (dim)
  pdim <- c(1, cumprod (dim))

  iarr <- matrix(NA, nrow = length(ivec), ncol = ndim) # matrix for the array indices
  colnames (iarr) <- letters[8 + seq_len (ndim)]       # i, j, k, ...

  ivec <- (ivec - 1)
  for (j in seq_len (ndim))
    iarr [, j] <- (ivec %% pdim [j + 1]) / pdim [j]

  1 + floor(iarr)
}

###-----------------------------------------------------------------------------
###
###  array2vec -- array index to vector index conversion
###
###

array2vec <- function (iarr, dim){
  if (!is.matrix (iarr))
    dim (iarr) <- c(1, length (iarr))

  if (ncol (iarr) != length (dim))
    stop ("Number of columns in iarr and number of dimensions differ.")

  if (any (sweep (iarr, 2, dim) > 0))
    stop ("array index > dim")

  pdim <- c(1, cumprod (dim [- length (dim)]))
  iarr <- iarr - 1

  colSums(apply (iarr, 1, "*", pdim)) + 1
}

###-----------------------------------------------------------------------------
###
###  array2df -- "explodes" a mulitdimensional array into a long form matrix or
###              data.frame. Compare stack, unstack.
###
###

array2df <- function (x, levels = rep (NA, length (dims)),
                      matrix = FALSE,
                      label.x = deparse (substitute (x))){
  dims  <- c(dim (x))
  cprod <- c(1, cumprod (dims))
  rprod <- c(rev (cumprod (rev (dims))), 1)[-1]
  idim  <- seq_len (length (dims)) [! sapply (levels, is.null)]

  df <- matrix (x, nrow = length (x), ncol = length (idim) + 1)

  for (d in seq (along = idim))
    df [, d + 1] <-  rep (seq_len (dims [idim [d]]), each = cprod [idim [d]], times = rprod [idim [d]])

  if(!matrix){
    df <- as.data.frame (df)

    for (d in seq (along = idim)){
      if (! all (is.na(levels[[idim [d]]]))){
        df[, d + 1] <- factor (df[, d + 1], labels = levels [[idim [d]]])
      }
    }

  }
  colnames (df) <- c (label.x, names (levels)[idim])

  df
}

###-----------------------------------------------------------------------------
###
###  matlab.palette
###
###

matlab.palette <- function (n = 100, ...) {
  rev (rainbow (n, start = 0, end = 4/6, ...))
}

###-----------------------------------------------------------------------------
###
###  matlab.dark.palette
###
###

matlab.dark.palette <- function (n = 100, ...) {
  pal <- rev (rainbow (n, start = 0, end = 4/6, ...))
  pal <- col2rgb(pal)
  pal ["green",] <- pal ["green",] / 2

  rgb (t (pal)/255)
}

###-----------------------------------------------------------------------------
###
### pearson.dist
###
###

pearson.dist <- function (x) {
  as.dist (0.5 - cor (t(x)) / 2)
}

###-----------------------------------------------------------------------------
###
### mean_pm_sd
###
###

mean_pm_sd <- function (x, na.rm = TRUE){
  m <- mean (x, na.rm = na.rm)
  s <- sd (x, na.rm = na.rm)
  c(m - s, m, m + s)
}

###-----------------------------------------------------------------------------
###
### mean_sd
###
###

mean_sd <- function (x, na.rm = TRUE)
  c(mean (x, na.rm = na.rm),  sd (x, na.rm = na.rm))

###-----------------------------------------------------------------------------
###
### wc - word count
###
###

wc <- function (file, flags = c("lines", "words", "bytes")){
  if (length (system ("wc --help", intern = TRUE)) == 0)
    return (NULL)

  wc <- paste ("wc", paste ("--", flags, sep = "", collapse = ", "), file)
  wc <- read.table(pipe (wc))
  colnames (wc) <- c(flags, "file")
  wc
}

###-----------------------------------------------------------------------------
###
### split.line - split line into list of key-value pairs
###
###

split.line <- function (x, separator, trim.blank = TRUE) {
  tmp <- regexpr (separator, x)
  #if (length (tmp) == 1 && tmp [[1]] == -1)
  #  warning ("line without separator", separator)

  key   <- substr (x, 1, tmp - 1)
  value <- substr (x, tmp + 1, nchar (x))

  if (trim.blank){
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    key <- sub (blank.pattern, "\\1", key)
    value <- sub (blank.pattern, "\\1", value)
  }

  value <- as.list (value)
  names (value) <- key

  value
}

###-----------------------------------------------------------------------------
###
### split.string - split string at pattern
###
###

split.string <- function (x, separator, trim.blank = TRUE, remove.empty = TRUE) {
  pos <- gregexpr (separator, x)
  if (length (pos) == 1 && pos [[1]] == -1)
    return (x)

  pos <- pos [[1]]

  pos <- matrix (c (1, pos + attr (pos, "match.length"),
                    pos - 1, nchar (x)),
                 ncol = 2)

  if (pos [nrow (pos), 1] > nchar (x))
    pos <- pos [- nrow (pos), ]

  x <- apply (pos, 1, function (p, x) substr (x, p [1], p [2]), x)

  if (trim.blank){
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    x <- sub (blank.pattern, "\\1", x)
  }

  if (remove.empty){
    x <- x [sapply (x, nchar) > 0]
  }

  x
}

###-----------------------------------------------------------------------------
###
### raw.split.nul - rawToChar conversion, splitting at \0
###
###

.nul <- as.raw (0)

raw.split.nul <- function (raw, trunc = c (TRUE, TRUE)) {
	# todo make better truncation
	trunc <- rep (trunc, length.out = 2)
	
	if (trunc [1] && raw [1] == .nul)
		raw <- raw [-1]
	if (trunc [2]) {
		tmp <- which (raw > .nul)
		if (length (tmp) == 0) 
			return ("")
		raw <- raw [1 : tail (tmp, 1)]	
	} 
	if (raw [length (raw)] != .nul)
		raw <- c (raw , .nul)
	
	tmp <- c (0, which (raw == .nul))
	
	out <- character (length (tmp) - 1)
	for (i in 1 : (length (tmp) - 1))
		if (tmp [i] + 1 < tmp [i + 1] - 1)
			out [i] <- rawToChar (raw [(tmp [i] + 1)  : (tmp [i + 1] - 1)])
	
	out
}

###-----------------------------------------------------------------------------
###
### factor2num - conversion of a factor containing numerical levels
###
###


factor2num <- function (f)
	as.numeric(levels (f)) [as.numeric (f)]

###-----------------------------------------------------------------------------
###
### getbynames - get list elements by name and if no such element exists, NA 
###
###

getbynames <- function (x, e) {
	x <- x [e]
	if (length (x) > 0) {
		if (is.character (e)) 
			names (x) <- e
		x [sapply (x, is.null)] <- NA
		x
	} else {
		list ()
	}
}
