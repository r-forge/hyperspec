###-----------------------------------------------------------------------------
###
###  aggregate
###
###

setMethod ("aggregate", "hyperSpec",
           function (x,
                     by = stop ("by is needed: either a grouping vector",
                       "(e.g. factor) or a list of groups"),
                     FUN = stop ("FUN is needed."),
                     ...,
                     out.rows = NULL, append.rows = NULL,
                     short = "aggregate", date = NULL, user = NULL){
  validObject (x)

  if (!is.list (by))
    by <- split (seq (x, index = TRUE), by, drop = TRUE)

  ## main work here is to avoid calling stats::aggregate as there splitting and
  ## rearranging is involved. That is slow with the spectra.

  # try a guess how many rows the result will have
  if (is.null (out.rows)){
    tmp <- .apply (data = x@data[by [[1]], , drop = FALSE], MARGIN = 2, FUN = FUN, ...)

    out.rows <- nrow (tmp) * length (by)
  }

  data  <- x@data[rep (1, out.rows), , drop = FALSE] # preallocate memory
  data <- cbind (data, .aggregate = NA)
  col.aggregate <- ncol (data)


  r <- 1 # keeping track of the actually filled rows

  for (i in seq (along = by)){
    tmp <- .apply (data = x@data[by [[i]], , drop  = FALSE], MARGIN = 2, FUN = FUN, ...)

    prows <- nrow (tmp) - 1


    if (r + prows > out.rows) {
      if (is.null (append.rows))
        append.rows <- max (100, ceiling (1 - (i / length (by)) * out.rows))
      out.rows <- max (append.rows + out.rows, r + prows)
      data  <- rbind (data, data [rep (1, out.rows - nrow (data)), , drop = FALSE])
      warning ("At", i, "of", length (by),
               "levels: Output data.frame too small. Consider using an",
               "appropriate value for out.rows to speed up calculations.")
    }

    if (prows >= 0){
      data[r : (r + prows), -col.aggregate] <- tmp
      data[r : (r + prows),  col.aggregate] <- i

      r <- r + prows + 1
    }
  }

  x@data <- data[seq_len (r - 1), , drop = FALSE]
  x@data[, col.aggregate] <- factor (x@data[, col.aggregate], levels = seq_len (length (by)))

  if (!is.null (names (by)) && !any (is.na (names (by))))
    levels (x@data[, col.aggregate]) <- names (by)

  .logentry (x,
             long = list (by = by, FUN = FUN, out.rows = out.rows,
               append.rows = append.rows, ...),
             short = short, date = date, user = user)
})
