## ggplot functions

qplotspc <- function (x, ..., spc.nmax = 50){

  chk.hy (x)
  validObject (x)

  if (nrow (x) > spc.nmax) {
    warning ("Number of spectra exceeds spc.nmax. Only the first ", spc.nmax, " are plotted.")
    x <- x [seq_len (spc.nmax)]
  }

  df <- as.long.df (x, rownames = TRUE)

  ggplot (df, aes (x = .wavelength, y = spc, groups = .rownames)) +
    xlab (x@label$.wavelength) + ylab (x@label$spc) +
    geom_line (...)
}
