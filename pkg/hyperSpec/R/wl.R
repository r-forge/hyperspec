###-----------------------------------------------------------------------------
###
###  wl
###
###

wl <- function (x){
  chk.hy (x)
  validObject (x)
  
  x@wavelength
}

###-----------------------------------------------------------------------------
###
###  .wl
###
###
".wl<-" <- function (x, digits = 6, value){
  x@wavelength <- value
  colnames (x@data$spc) <- signif (value, digits)

  x
}

###-----------------------------------------------------------------------------
###
###  wl<-
###
###

"wl<-" <- function (x, label = NULL, digits = 6, short = "wl<-", user = NULL, date = NULL, value){

  chk.hy (x)
  validObject (x)

  if (is.numeric (value)){
    if (is.null (label))
      warning ("Do not forget to adjust the label of the wavelength axis.")
  } else if (is.list (value)){
    label <- value$label
    value <- value$wl
  }

  .wl (x) <- value

  x@label$.wavelength <- label

  validObject (x)
  .logentry (x, short = short, long = list (value = value, digits = digits),
             date = date, user = user)
}
