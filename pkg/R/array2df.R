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
