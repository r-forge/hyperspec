###-----------------------------------------------------------------------------
###
###  apply
###
###

setMethod ("apply", "hyperSpec", function (X, MARGIN, FUN, ...,
                                           label.wl = NULL, label.spc = NULL, new.wavelength = NULL,
                                           short = "apply", long = NULL, user = NULL, date = NULL){
  validObject (X)

  if (is.null (long))
    long <- .call.list (match.call (call = sys.call (-1)))

  if (missing (MARGIN)){                # apply for functions that the complete spectra matrix
    ## is easier: tmp <- apply (x, , FUN, ...)
    ## does: 
    ## tmp <- x
    ## tmp [[]] <- FUN (x [[]], ...)

    X@data$spc <- do.call (FUN, list (X@data$spc, ...))
    
  } else if (all (MARGIN == 1 : 2)){    # apply for functions that take scalar arguments. 

    tmp <- apply (X@data$spc, MARGIN = MARGIN, FUN, ...)
    tmp <- as.numeric (tmp)             # otherwise surprises will be waiting

    dim (tmp) <- dim (X@data$spc)       

    X@data$spc <- tmp
    
  } else {
    ## the usual: for each row / for each column

    X@data <- .apply(X@data, MARGIN = MARGIN, FUN = FUN, ...)

    if (all (MARGIN == 1)) {
      
      ## if the number of data points per spectrum is changed, the wavelength vector needs to be
      ## adapted, too
      
      if (ncol (X@data$spc) != length (X@wavelength)) {
        
        ## only internal functions here: the validation will fail until the wavelength axis is
        ## adjusted
        
        if (!is.null (new.wavelength)){    # vector with new wavelength is given
          if (is.numeric (new.wavelength)) # either directly,
            .wl (X) <- new.wavelength
          else {
            dots <- list (...)
            .wl (X) <- dots [[new.wavelength]] # or as name of the argument that becomes the new
                                               # wavelength vector
          }
        } else if (ncol (X@data$spc) != length (X@wavelength)){
          wl <- as.numeric (colnames (X@data$spc)) # if not given, try to make from colnames of the 
                                                   # spectra matrix
        
          if (length (wl) != ncol (X@data$spc) || any (is.na (wl)))
            wl <- seq_len (ncol (X@data$spc)) # or just number sequentially

          .wl (X) <- wl
        }
      }
    }
  }

  if (!is.null (label.wl))
    X@label$.wavelength <- label.wl

  if (!is.null (label.spc))
    X@label$spc <- label.spc

  validObject (X)

  .logentry(X, short = short, long = long, user = user, date = date)
})



