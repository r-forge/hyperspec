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

"labels<-" <- function (object, which = NULL, ..., value){
  .is.hy (object)
  validObject (object)

  if (is.null (which))
    object@label <- value
  else {
    if ((is.character (which) && !which %in% colnames (object@data)) && which != ".wavelength" ||
        (is.numeric (which) && (which < 1 || which > ncol (object@data) + 1)) ||
        (is.logical (which) && length (which) != ncol (object@data) + 1)
        )
      stop ("Label does not exist!")

    object@label [[which]] <- value

  }

  object <- .logentry (object,
                       long = list (which = which, value = value),
                       ...)

  validObject (object)

  object
}

