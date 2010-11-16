###-----------------------------------------------------------------------------
###
###  read.txt.wide
###
###  Format:
###  x y ... int (wl1)  int (wl2) ... int (wl p) z ...
###
read.txt.wide <- function (file = stop ("file is required"),
                           cols = list (
                             spc = "I / a.u.",
                             .wavelength = expression (lambda / nm)),
                           check.names = FALSE,
                           ...){
  txtfile <- read.table (file = file, ..., check.names = FALSE)

  .wavelength <- match (".wavelength", names (cols))
  if (is.na (.wavelength))
    cols <- as.list (c (cols, .wavelength = expression (lambda / nm)))
  else
    if (.wavelength != length (cols))   # .wavelength should be at the end of cols
      cols <- cols [c (seq_along (cols)[-.wavelength], .wavelength)]

  ## columns containing the spectra
  spc <- match ("spc", names (cols))
  if (is.na (spc))
    stop ("cols$spc must exist.")

  spc <- 0 : (ncol (txtfile) - length (cols) + 1) + spc

  spc.data <- as.matrix (txtfile[, spc])
  txtfile$spc <- I (spc.data)
  txtfile <- txtfile [, -spc, drop = FALSE]


  new ("hyperSpec",
       data = txtfile,
       label = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       )
}
