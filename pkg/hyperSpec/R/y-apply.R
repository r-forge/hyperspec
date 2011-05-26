###-----------------------------------------------------------------------------
###
###  .apply
###
###

.apply <- function (data, MARGIN, FUN, ...){

  if (length (data$spc) == 0)
    stop ("empty spectra matrix.")

  spc <- apply (data [, "spc", drop = FALSE], MARGIN, FUN, ...)

  if (MARGIN == 1){
    if (is.null (spc))
      spc <- matrix (ncol = 0, nrow = nrow (data))
    else if (is.vector (spc))
      dim (spc) <- c(length (spc), 1)
    else if (is.matrix (spc))
      spc <- t (spc)

    data$spc <- I(spc)
  } else if (MARGIN == 2){
    if (is.null (spc))
      return (data [0, ])
    if (is.null (dim (spc)))
      dim (spc) <- c(1, ncol (data$spc))

    if (all(dim (spc) == dim (data$spc))){
      data$spc <- spc
    }  else {
      nrow <- nrow (spc)

      data <- data[rep (1, nrow), , drop = FALSE]

      cols <- colnames (data)
      cols <- which (cols != "spc")
      if (length (cols) > 0) {
        colvals <- apply (data [,cols,drop = FALSE], 2, .na.if.different)
        data [,cols] <- rep (colvals, each = nrow)
      }

      data$spc <- I (spc)
      rownames (data) <- rownames (spc)
    }
  }

  data
}
