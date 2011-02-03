###-----------------------------------------------------------------------------
###
###  labels
###
###

.labels <- function (object, which = bquote(), drop = TRUE, ..., use.colnames = TRUE){
  validObject (object)                  

  if (! missing (which) & ! is.character (which))
    warning ("labels are not guaranteed to have the same order as columns.",
             "Consider using indexing by name.")

  ## fill in colnames?
  if (use.colnames){                            
    label <- structure (as.list (c(colnames (object@data), ".wavelength")),
                        names = c(colnames (object@data), ".wavelength"))
    label <- modifyList (label, object@label)

    label <- label [which]
  } else {
    label <- object@label [which]
  }
  
  if (drop && length (label) == 1L)
    label <- label [[1]]

  label
}

test (.labels) <- function (){
  checkEquals (labels (flu), flu@label)

  tmp <- flu
  tmp@label$file <- NULL
  checkEquals (labels (tmp, c ("c", "file", "fil"), use.colnames = FALSE),
               structure(list(c = "c / (mg / l)", `NA` = NULL, `NA` = NULL),
                         .Names = c("c", NA, NA)))

  checkEquals (labels (tmp, c ("c", "file", "fil")),
               structure(list(c = "c / (mg / l)", file = "file", `NA` = NULL),
                         .Names = c("c", "file", NA)))
}

setMethod ("labels", "hyperSpec", .labels)

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

