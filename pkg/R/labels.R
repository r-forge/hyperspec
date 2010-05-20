###-----------------------------------------------------------------------------
###
###  labels
###
###

setMethod ("labels", "hyperSpec", function (object, which = NULL, drop = TRUE, ...){
  validObject (object)

  if (is.null (which))
    object@label
  else {
    label <- object@label [which]

    if (drop && (length (label) == 1))
      label <- label [[1]]

    label
  }
})

###-----------------------------------------------------------------------------
###
###  labels<-
###
###

"labels<-" <- function (object, which = NULL, ...,
                        short = "labels<-", user = NULL, date = NULL,
                        value){
  chk.hy (object)
  validObject (object)

  if (is.null (which))
    object@label <- value
  else {
    if ((is.character (which) && !which %in% colnames (object@data)) &&
         which != ".wavelength" ||      # neither a colname nor .wavelength
        (is.numeric (which) && (which < 1 || which > ncol (object@data) + 1)) ||
        (is.logical (which) && length (which) != ncol (object@data) + 1)
                                        # or outside valid indices
        )
      stop ("Column to label does not exist!")

    object@label [[which]] <- value

  }

  validObject (object)
  .logentry (object, short = short, long = list (which = which, value = value),
             user = user, date = date)
}

