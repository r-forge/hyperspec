###-----------------------------------------------------------------------------
###
###  decomposition - decompose hyperSpec object e.g. into scores and loading objects
###

decomposition <- function (object, x, wavelength = seq_len (ncol (x)),
                           label.wavelength, label.spc,
                           scores = TRUE, retain.columns = FALSE,
                           short = "decomposition", user = NULL, date = NULL,
                           ...){
  validObject (object)

  if (is.vector (x))
    if (nrow (object) == length (x))
      dim (x) <- c(length (x), 1)
    else
      dim (x) <- c(1, length (x))

  if ((nrow (x) == nrow (object)) && scores){

    object@data$spc <- I (as.matrix (x))

    .wl (object) <- wavelength

    object@label$.wavelength <- label.wavelength

  } else if (ncol (x) == nwl (object)){
    spc <- match("spc", colnames(object@data))
    
    ## apply changes type of retained columns to character!!!
    ## cols <- apply(object@data[, -spc, drop = FALSE], 2, .na.if.different)
    ## object@data[1, -spc] <- cols
    cols <- apply (object@data [, -spc, drop = FALSE], 2, function (x) length (unique (x)))
    is.na (object@data[1, -spc]) <- cols > 1
    
    if (!retain.columns) {
      cols <- which (cols > 1)
      cols[cols >= spc] <- cols [cols >= spc] + 1
      object@data <- object@data[, -cols, drop = FALSE]
    }

    object@data <- object@data[rep(1, nrow(x)), , drop = FALSE]
    object@data$spc <- I(as.matrix(x))
  } else {
    stop ("Either rows (if scores == TRUE) or columns (if scores == FALSE) of",
          " x and object must correspond")
  }

  object@label$spc <- label.spc
  
  validObject (object)

  .logentry (object, short = short,  user = user, date = date)
}
