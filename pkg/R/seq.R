#################################################################################
###
###  seq.R - seq Method for hyperSpec objects
###  Time-stamp: <Claudia Beleites on Monday, 2010-01-25 at 17:37:30 on cb>
###  
###  generates index seqences
###  
###  Version 1.0  2010-01-25 16:46  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

## needs to be an S3 function as S4 ... dispatch has to have the same signature
## for all parameters
seq.hyperSpec <- function (x, ..., along.with = "spc"){
  validObject (x)
  dots <- list (...)
  switch (along.with,
          spc = {
            dots <- modifyList (list (from = 1, to = nrow (x)), dots)
            do.call (seq.int, dots)
          },
          wl = {
            dots <- modifyList (list (from = 1, to = nwl (x)), dots)
            wl (x) [do.call (seq.int, dots)]
          },
          wli = {
            dots <- modifyList (list (from = 1, to = nwl (x)), dots)
            do.call (seq.int, dots)
          },
          stop ("Unknown along.with: ", along.with))
}
