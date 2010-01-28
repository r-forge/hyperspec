###-----------------------------------------------------------------------------
###
###  orderwl - order the wavelength axis ascending
###
orderwl <- function (x, na.last = TRUE, decreasing = FALSE,
                     short = "orderwl", date = NULL, user = NULL){
  .is.hy (x)
  validObject (x)
  
  ord <- order (x@wavelength, na.last = na.last, decreasing = decreasing)

  if (any (ord != seq_along (x@wavelength))){
    x@data$spc <-  x@data$spc [, ord, drop = FALSE]
    .wl(x) <- x@wavelength [ord]
  }

  .logentry (x, short = short,
             long = list (na.last = na.last, decreasing = decreasing),
             date = date, user = user)
}
