###-----------------------------------------------------------------------------
###
### replacing with [[<-
###

setReplaceMethod ("[[", "hyperSpec",
                  function (x, i, j, l, wl.index = FALSE,
                            short = "[[<-", user = NULL, date = NULL,
                            ..., value){
  validObject (x)

  long <- list (i = if (missing (i)) "" else i ,
                l = if (missing (l)) "" else l,
                wl.index = wl.index,
                ...,
                value = if (is (value, "hyperSpec")) as.character (value)
                else .paste.row (value, val = TRUE)
                )

  if (! missing (j))
    stop ("The spectra matrix may only be indexed by i (spectra) and l",
          " (wavelengths). j (data column) must be missing.")

  if  (!missing (l) && !wl.index)
    l <- wl2i (x, l)

  if (is (value, "hyperSpec")){
    validObject (value)
    value <- value@data$spc
  }

  x@data$spc[i, l, ...] <- value

  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
})
