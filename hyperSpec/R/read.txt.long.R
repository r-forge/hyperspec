###-----------------------------------------------------------------------------
###
###  read.txt.long: import measurements from .txt file
###
###  Format:
###  (y x) wl int
###

read.txt.long <- function (file = stop ("file is required"),
                           cols = list (
                             .wavelength = expression (lambda / nm),
                             spc = "I / a.u."),
                           header = TRUE,
                           ...){
  txtfile <- read.table (file = file, header = header, ...)

  if (header){
    cln <- match (colnames (txtfile), names (cols))
    cln <- cols[cln]
    names (cln) <- colnames (txtfile)
    cols <- cln
    rm (cln)
  } else {
    if (ncol (txtfile) != length (cols)){
      warning (paste ("cols does not correspond to the columns in", file,
                      ". Guessing remaining columns."))
      cols <- c (character (ncol (txtfile) - 2), cols)
    }
  }


  if (is.na (match ("spc", names (cols))))
    stop ("cols$spc must exist.")

  wavelength <- match (".wavelength", names (cols))
  if (is.na (wavelength))
    stop ("cols$.wavelength must exist.")

  colnames (txtfile) <- names (cols)

  ## wavelength axis
  wavelength <- as.numeric (levels (as.factor (txtfile$.wavelength)))
  
  spc <- as.matrix (unstack (txtfile, form = spc ~ .wavelength))
  if ((nrow (spc)  == length (wavelength)) & (ncol (spc) != length (wavelength)))
    spc <- t (spc)

  colnames (spc) <- levels (txtfile$.wavelength)

  txtfile <- txtfile [txtfile$.wavelength == txtfile$.wavelength[1], ]
  txtfile$.wavelength <- NULL
  txtfile$spc <- I (spc)

  new ("hyperSpec",
       wavelength = wavelength,
       data = txtfile,
       labels = cols,
       log = list (
         short = "read.txt.long",
         long = list (file = file, cols = I (cols), ...)
         )
       )
}
