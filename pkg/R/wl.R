###-----------------------------------------------------------------------------
###
###  wl
###
###

wl <- function (x){
  .is.hy (x)
  validObject (x)
  
  x@wavelength
}

###-----------------------------------------------------------------------------
###
###  .wl
###
###
".wl<-" <- function (x, digits = 6, value){
  x@wavelength <- value
  colnames (x@data$spc) <- signif (value, digits)

  x
}

###-----------------------------------------------------------------------------
###
###  wl
###
###

"wl<-" <- function (x, label = NULL, digits = 6, short = "wl<-", user = NULL, date = NULL, value){

  .is.hy (x)
  validObject (x)

  if (is.numeric (value)){
    if (is.null (label))
      warning ("Do not forget to adjust the label of the wavelength axis.")
  } else if (is.list (value)){
    label <- value$label
    value <- value$wl
  }

  .wl (x) <- value

  x@label$.wavelength <- label

  validObject (x)
  .logentry (x, short = short, long = list (value = value, digits = digits),
             date = date, user = user)
}

###-----------------------------------------------------------------------------
###
###  orderwl - order the wavelength axis ascending
###
orderwl <- function (x, na.last = TRUE, decreasing = FALSE,
                     short = "orderwl", date = NULL, user = NULL){
  .is.hy (x)
  validObject (x)
  
  ord <- order (x@wavelength, na.last = na.last, decreasing = decreasing)
  if (any (ord != seq_len (length (x@wavelength)))){
    x@data$spc <-  x@data$spc [, ord, drop = FALSE]
    .wl(x) <- x@wavelength [ord]
  }

  .logentry (x, short = short,
             long = list (na.last = na.last, decreasing = decreasing),
             date = date, user = user)
}


###-----------------------------------------------------------------------------
###
### wl2i
###
###
wl2i <- function (x, wavelength = stop ("wavelengths are required.")){
  .is.hy (x)
  validObject (x)

  ## special in formula
  max <- max (x@wavelength)
  min <- min (x@wavelength)

  `~` <- function (e1, e2){
    if (missing (e2))              # happens with formula ( ~ end)
      stop ("wavelength must be a both-sided formula")

    if (    (Re (e1) < min (x@wavelength) && Re (e2) < min (x@wavelength)) ||
        (Re (e1) > max (x@wavelength) && Re (e2) > max (x@wavelength))){
      NULL                       # wavelengths completely outside the wl. range of x
    } else {
      e1 <- .getindex (x, Re (e1)) + Im (e1)
      e2 <- .getindex (x, Re (e2)) + Im (e2)

      if (e1 <= 0 || e2 <= 0|| e1 > length (x@wavelength) || e2 > length (x@wavelength))
        warning ("wl2i: formula yields indices outside the object.")

      seq (e1, e2)
    }
  }

  .conv.range <- function (range){
    if (is.numeric (range)){
      .getindex (x, range, extrapolate = FALSE)
    } else
    eval (range)
  }

  if (is.list (wavelength))
    unlist (lapply (wavelength, .conv.range))
  else (.conv.range (wavelength))
}
###-----------------------------------------------------------------------------
###
### i2wl
###
###

i2wl <- function (x, i){
  .is.hy (x)
  validObject (x)

  x@wavelength[i]
}

###-----------------------------------------------------------------------------
###
### .getindex
###
###
## does the acual work of looking up the index
## extrapolate = TRUE returns first resp. last index for wavelength outside hyperSpec@wavelength.
## extrapolate = FALSE returns NA in this case

.getindex <- function (x, wavelength, extrapolate = TRUE){
    if (! extrapolate) {
        wavelength [wavelength < min (x@wavelength)] <- NA
        wavelength [wavelength > max (x@wavelength)] <- NA
    }
    tmp <- wavelength [! is.na (wavelength)]
    if (length (tmp) > 0) {
        tmp <- sapply (tmp,
                         function (x, y) which.min (abs (x  - y)),
                         x@wavelength)
        wavelength [! is.na (wavelength)] <- tmp
    }
    wavelength
}
