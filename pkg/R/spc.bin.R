###-----------------------------------------------------------------------------
###
###  spc.bin
###

spc.bin <- function (spc, by = stop ("reduction factor needed"), na.rm = TRUE,
                     short = "spc.bin", user = NULL, date = NULL) {
  .is.hy (spc)
  validObject (spc)

  long <- list (by = deparse (by), na.rm = na.rm)

  n <- ceiling (nwl (spc) / by)

  small <- nwl (spc) %% by
  if (small != 0)
    warning (paste (c("Last data point averages only ", small, " points.")))

  bin <- rep (seq_len (n), each = by, length.out = nwl (spc))

  na <- is.na (spc@data$spc)

  if ((na.rm > 0) && any (na)) {
    if (na.rm == 1) {
      na <- apply (!na, 1, tapply, bin, sum, na.rm = FALSE)
      spc@data$spc <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = TRUE) / na)
    } else { # faster for small numbers of NA
      tmp <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
      tmp <- sweep (tmp, 2, rle (bin)$lengths, "/")

      na <- which (is.na (tmp), arr.ind = TRUE)
      bin <- split (wl.seq (spc), bin)

      for (i in seq_len (nrow (na))){
        tmp [na [i, 1], na [i, 2]] <- mean (spc@data$spc [na [i, 1], bin [[na[i, 2]]]], na.rm = TRUE)
      }
      spc@data$spc <- tmp
    }
  } else {  # considerably faster
    spc@data$spc <- t (apply (spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
    spc@data$spc <- sweep (spc@data$spc, 2, rle (bin)$lengths, "/")
  }

  .wl (spc) <- as.numeric (tapply (spc@wavelength, bin, mean, na.rm = na.rm > 0))

  validObject (spc)
  .logentry (spc, short = short, long = long, user = user, date = date)
}

