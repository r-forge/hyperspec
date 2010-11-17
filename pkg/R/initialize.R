###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
###  C. Beleites
###

.initialize <- function (.Object, spc = NULL, data = NULL, wavelength = NULL, label = NULL, log = NULL,
                         ## ...,
                         short = "initialize", user = NULL, date = NULL){

  long <- list (data       = if (missing (data))       "missing" else .paste.row (data, val = TRUE),
                spc        = if (missing (spc))        "missing" else .paste.row (spc, val = TRUE,
                  range = FALSE),
                wavelength = if (missing (wavelength)) "missing" else wavelength,
                label      = if (missing (label))      "missing" else label)
  
        
  if (! is.null (data$spc) && ! (is.null (spc)))
    warning ("Spectra in data are overwritten by argument spc.")
        
  ## deal with spectra
  if (is.null (spc)){
    if (is.null (data$spc)){
      spc <- structure(numeric (0), .Dim = c(0L, 0L))
    } else {
      spc <- data$spc
    }
  } else if (!is.matrix (spc)) {
    spc <- structure (spc, dim = c (1L, length (spc)), # use spc as row vector
                      dimnames = list (NULL, names (spc))) 
  } 

  ## deal with extra data
  if (is.null (data)){
    data <- data.frame (spc = I (spc))
  } else {
    data$spc <- I (spc)
  }
  data$spc <- unclass (data$spc)

  .Object@data <- data

  ## now the wavelength axis
  if (is.null (wavelength)){
    ## guess from spc's colnames
    wavelength <- as.numeric (colnames (spc))
    
    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- seq_len (ncol (spc)) # guessing didn't work
  } 
  .wl (.Object) <- wavelength
  
  ## column + wavelength axis labels
  if (is.null (label) || length (label) == 0L)
    label <- structure (vector ("list", length (colnames (.Object@data)) + 1),
                                        # only for named columns! not ncol
                        .Names = c(".wavelength", colnames (.Object@data)))
  
  ## transform them all into expressions
  .make.expression <- function (x){	
    if (is.language (x) && ! is.expression (x))
      class (x) <- "expression"
    else if (is.character (x))
      x <- as.expression (x)
    x
  }
  
  label <- lapply (label, .make.expression)
  
  .Object@label <- label
  
  ## even the logbook may be given...
  if (is.data.frame (log)) {
    .Object@log <- log
  } else {
    .Object@log <- data.frame ()
    if (is.null (log))
      log <- list (short = short, long = long, user = user, date = date)
    
    .Object <- .logentry (.Object, .entry = log)
  }
  
  ## finally: check whether we got a valid hyperSpec object
  validObject (.Object)

  .Object
}

setMethod ("initialize", "hyperSpec", .initialize)

test (.initialize) <- function (){

  checkEqualsNumeric (dim (new ("hyperSpec")), c (0L, 1L, 0L))

  h <- new ("hyperSpec", spc = 1 : 4)
  checkEqualsNumeric (h@data$spc, 1 : 4)
  checkEqualsNumeric (dim (h), c (1L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, 1 : 4)
  

  spc <- matrix (c(1 : 12), nrow = 3)
  h <- new ("hyperSpec", spc = spc)
  checkEqualsNumeric (h@data$spc, spc)
  checkEqualsNumeric (dim (h), c (3L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, 1 : 4)

  colnames(spc) <- c(600, 601, 602, 603)
  h <- new ("hyperSpec", spc = spc)
  checkEqualsNumeric (h@data$spc, spc)
  checkEqualsNumeric (dim (h), c (3L, 1L, 4L))
  checkEqualsNumeric (h@wavelength, c(600, 601, 602, 603))
}
