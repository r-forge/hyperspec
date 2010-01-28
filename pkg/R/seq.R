###------------------------------------------------------------------------------
###
###  seq
###


## needs to be an S3 function as S4 ... dispatch has to have the same signature
## for all parameters

seq.hyperSpec <- function (x, from = 1, to = nrow (x),
                           ..., index = FALSE,
                           short = "seq", user = NULL, date = NULL){
  validObject (x)

  s <- seq (from = from, to = to, ...)

  if (index)
    s
  else {
    x <- .extract (x, i = s)

    .logentry (x, short = short,
               long = list (from = from, to = to, ..., index = index),
               user = user, date = date)
  }

}
