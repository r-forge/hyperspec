###-----------------------------------------------------------------------------
###
###  initialize -- initialization, called by new ("hyperSpec", ...)
###
###  C. Beleites
###

.initialize <- function (.Object, spc = NULL, data = NULL, wavelength = NULL, labels = NULL, log = NULL,
                         ## ...,
                         short = "initialize", user = NULL, date = NULL){

  
  if (is.null (log) && .options$log)    # avoid if no log is needed: involves copy of data
    long <- list (data       = if (missing (data))       "missing" else .paste.row (data, val = TRUE),
                  spc        = if (missing (spc))        "missing" else .paste.row (spc, val = TRUE,
                    range = FALSE),
                  wavelength = if (missing (wavelength)) "missing" else wavelength,
                  labels      = if (missing (labels))      "missing" else labels)
  else
    long <- list ()

  ## do the small stuff first, so we need not be too careful about copies

  ## the wavelength axis
  if (!is.null (spc))
    nwl <- ncol (spc)
  else if (!is.null (data$spc))
    nwl <- ncol (data$spc)
  else
    nwl <- 0

  
  if (is.null (wavelength)){
    ## guess from spc's colnames
    if (!is.null (spc))
      wavelength <- as.numeric (colnames (spc))
    
    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- as.numeric (colnames (data$spc))
    
    if (length (wavelength) == 0L || any (is.na (wavelength)))
      wavelength <- seq_len (nwl) # guessing didn't work
  } 
  .Object@wavelength <- wavelength
  
  ## column + wavelength axis labels
  if (is.null (labels) || length (labels) == 0L){
    cln <- c (colnames (data), '.wavelength')
    if (! any (grepl ("spc", cln)))
      cln <- c (cln, "spc")
    labels <- vector ("list", length (cln))
    names (labels) <- cln
    rm (cln)
  }
  
  ## transform labels into expressions
  .make.expression <- function (x){	
    if (is.language (x) && ! is.expression (x))
      class (x) <- "expression"
    else if (is.character (x))
      x <- as.expression (x)
    x
  }

  labels <- lapply (labels, .make.expression)
  
  .Object@label <- labels

  rm (labels, wavelength)
  if (.options$gc) gc ()
  
  ## even the logbook may be given...
  if (is.data.frame (log)) {
    .Object@log <- log
  } else {
    .Object@log <- data.frame ()
    if (is.null (log))
      log <- list (short = short, long = long, user = user, date = date)
    
    .Object <- .logentry (.Object, .entry = log)
  }
  rm (log)
  if (.options$gc) gc ()
        
  if (! is.null (data$spc) && ! (is.null (spc)))
    warning ("Spectra in data are overwritten by argument spc.")
  
  ## deal with spectra
  if (is.null (spc) && is.null (data$spc)){
    spc <- structure(numeric (0), .Dim = c(0L, 0L))
  } 

  if (! is.null (spc) && !is.matrix (spc)) {
    spc <- structure (spc,
                      dim = c (1L, length (spc)), # use spc as row vector
                      dimnames = list (NULL, names (spc))) 
  } 
  if (.options$gc) gc ()

  if (! is.null (spc)){
    attr (spc, "class") <- "AsIs"       # I seems to make more than one copy
    if (.options$gc) gc ()
  }
  
  ## deal with extra data
  if (is.null (data)){
    data <- data.frame (spc = spc)
  } else if (is.null (data$spc)){
    data$spc <- spc
  }
  rm (spc)
  if (.options$gc) gc ()
  
  attr (data$spc, "class") <- NULL      # more than one copy!?
  if (.options$gc) gc ()

  .Object@data <- data
  if (.options$gc) gc ()
  
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
