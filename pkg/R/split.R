###-----------------------------------------------------------------------------
###
### split - split according to given factor
###

setMethod ("split", "hyperSpec", function (x, f, drop = TRUE, #...,
                                           short = NULL, user = NULL, date = NULL){
  validObject (x)

  hyperlist <- split (seq_len (nrow (x@data)), f, drop)

  log <-  .logentry (x, short = short, long = list (f = f, drop = drop),
                    user = user, date = date)@log

  for (i in seq_len (length (hyperlist))){
    hyperlist[[i]] <- x[hyperlist[[i]],]

    hyperlist[[i]]@log <- log
  }

  hyperlist
})
