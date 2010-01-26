###-----------------------------------------------------------------------------
###
### cbind2
###
###
setMethod ("cbind2", signature (x = "hyperSpec", y  = "hyperSpec"),
           function (x, y){
             validObject (x)
             validObject (y)

             cols <- match (colnames (x@data), colnames (y@data))
             cols <- colnames(y@data)[cols]
             cols <- cols[!is.na (cols)]
             cols <- cols [-match ("spc", cols)]

             if (length (cols) < 0){
               ord <- do.call (order, x@data[, cols, drop = FALSE])
               x@data <- x@data[ord, , drop = FALSE]

               ord <- do.call (order, y@data[, cols, drop = FALSE])
               y@data <- y@data[ord, , drop = FALSE]

               if (any (x@data[, cols, drop = FALSE] != y@data[, cols, drop = FALSE]))
                 stop ("hyperSpec objects must have the same data (except data$spc)")
             }

             x@data$spc <- cbind(x@data$spc, y@data$spc)
             .wl (x) <- c (x@wavelength, y@wavelength)

             x@data <- cbind (x@data,
                              y@data[, is.na (match (colnames (y@data), colnames (x@data))), drop = FALSE])

             .logentry (x, short = "cbind2", long = as.character (y))
           }
           )

setMethod("cbind2", signature (x = "hyperSpec", y = "missing"), function (x, y)x)

###-----------------------------------------------------------------------------
###
### rbind2
###
###
setMethod("rbind2",
          signature(x = "hyperSpec", y = "hyperSpec"),
          function (x, y) {
            validObject (x)
            validObject (y)

            if (! isTRUE (all.equal (x@wavelength, y@wavelength)))
              stop ("The wavelengths of the objects differ.")

            x@data <- rbind (x@data, y@data)

            .logentry (x, short = "rbind2", long = list (y = as.character (y)))
          }
          )

setMethod ("rbind2", signature (x = "hyperSpec", y = "missing"), function (x, y) x)

###-----------------------------------------------------------------------------
###
### cbind & rbind
###
###
bind <- function (direction = stop ("direction ('c' or 'r') required"),
                  ..., short = NULL, user = NULL, date = NULL){
  dots <- list (...)

  if ((length (dots) == 1) & is.list (dots [[1]]))
    dots <- dots[[1]]

  if (length (dots) == 0)
    NULL
  else if (length (dots) == 1){
    validObject (dots[[1]])
    dots[[1]]
  } else {
    if (! is (dots[[1]], "hyperSpec"))
      stop ("bind only works on hyperSpec objects.")

    logs <- list()

    for (i in seq_len (length (dots)) [-1]){
      if (! is (dots[[i]], "hyperSpec"))
        stop ("bind only works on hyperSpec objects.")
      validObject (dots[[i]])

      dots[[1]] <- switch (direction,
                           c = cbind2 (dots[[1]], dots[[i]]),
                           r = rbind2 (dots[[1]], dots[[i]]),
                           stop ("direction must be either 'c' or 'r' for cbind and rbind, respectively.")
                           )

      if (!is.null (short))
        dots[[1]]@log[nrow (dots[[1]]@log), "short.description"] <- paste (short, dots[[1]]@log[nrow (dots[[1]]@log), "short.description"])
      if (!is.null (date))
        dots[[1]]@log[nrow (dots[[1]]@log), "date"] <- date
      if (!is.null (user))
        dots[[1]]@log[nrow (dots[[1]]@log), "user"] <- user
    }
    dots[[1]]
  }
}

cbind.hyperSpec <- function (..., deparse.level) bind ("c", ...)
rbind.hyperSpec <- function (..., deparse.level) bind ("r", ...)

