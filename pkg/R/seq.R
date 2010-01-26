#################################################################################
###
###  seq.R - seq Method for hyperSpec objects
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-01-26 at 17:47:50 on cb>
###  
###  generates index seqences over the spectra
###  
###  Version 1.0  2010-01-25 16:46  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

## needs to be an S3 function as S4 ... dispatch has to have the same signature
## for all parameters

seq.hyperSpec <- function (x, from = 1, to = nrow (x),
                           ..., index = FALSE,
                           short = "seq", name = NULL, date = NULL){
  validObject (x)

  s <- seq.int (from = from, to = to, by = by, length.out = length.out)

  if (index)
    s
  else {
    x <- .extract (x, i = s)

    .logentry (x, short = short, long = .call.list (), user = user, date = date)
  }

}
