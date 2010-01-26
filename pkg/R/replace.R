###-----------------------------------------------------------------------------
###
### replacing with [<-
###

setReplaceMethod("[", "hyperSpec",
                 function (x, i, j,
                           short = "[<-", user = NULL, date = NULL,
                           ..., value){
  validObject (x)

  long <- list (i = if (missing (i)) "" else i ,
                j = if (missing (j)) "" else j,
                drop = drop,
                ...,
                value = if (is (value, "hyperSpec"))
                           as.character (value)
                        else
                           .paste.row (value, val = TRUE)
                )

  if (missing (i)) i <- seq_len (nrow (x@data))
  if (missing (j)) j <- seq_len (ncol (x@data))

  if (is (value, "hyperSpec")){
    validObject (value)
    x@data [i, j, ...] <- value@data
  } else {
    x@data [i, j, ...] <- value
  }

  validObject (x)

  .logentry (x, short = short, long = long, date = date, user = user)
})

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

###-----------------------------------------------------------------------------
###
### subassignname - subassigning with $
###

setReplaceMethod ("$", "hyperSpec", function (x, name, value){
  validObject (x)

  if (is.list (value) && (length (value) == 2)){
    ilabel <- match ("label", names (value))

    if (is.na (ilabel))
      ilabel <- 2

    label <- value [[ilabel]]

    value <- value [[3 - ilabel]] ## the other of the 2 entries

  } else {
    label <- name
  }

  if (name == "..") { ## shortcut
    i <- -match ("spc", colnames (x@data))
    x@data[, i] <- value

    if (!is.null (label)){
      i <- colnames (x@data)[i]
      i <- match (i, names (x@label))
      x@label[i] <- label
    }
  } else {
    dots <- list (x = x@data, name = name, value = value)
    x@data <- do.call("$<-", dots)
    x@label[[name]] <- label
  }

  .logentry (x, short = "$<-", 
             long = list (name = name, value = .paste.row (value, val = TRUE)))
})

