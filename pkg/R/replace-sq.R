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
