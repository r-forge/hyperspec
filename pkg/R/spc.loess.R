###-----------------------------------------------------------------------------
###
###  spc.loess
###
###

spc.loess <- function (spc, newx, enp.target = nwl (spc) / 4,
                       surface = "direct", ...,
                       short = "spc.loess", user = NULL, date = NULL){

  .loess <- function (y, x){
    if (all (is.na (y)))
      NA
    else
      loess (y ~ x, enp.target = enp.target, surface = surface, ...)
  }

  .predict <-  function (loess, x){
    if (!is (loess, "loess") && is.na (loess))
      rep (NA_real_, length (x))
    else
      predict (loess, x)
  }

  
  chk.hy (spc)
  validObject (spc)

  if (any (newx < min (spc@wavelength)) || any (newx > max (spc@wavelength)))
    warning ("newx outside spectral range of spc. NAs will be generated.")

  loess <- apply (t (spc[[]]), 2, .loess, spc@wavelength)

  spc@data$spc <- t (sapply (loess, .predict, newx))
  .wl(spc) <- newx

  .logentry (spc, short =  short,
             long = list (newx = newx, enp.target = enp.target,
               surface = surface, ...),
             user = user, date = date)
}
