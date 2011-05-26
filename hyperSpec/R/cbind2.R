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
             cols <- colnames (y@data) [cols]
             cols <- cols [! is.na (cols)]
             cols <- cols [- match ("spc", cols)]

             if (length (cols) < 0){
               ord <- do.call (order, x@data[, cols, drop = FALSE])
               x@data <- x@data[ord, , drop = FALSE]

               ord <- do.call (order, y@data[, cols, drop = FALSE])
               y@data <- y@data[ord, , drop = FALSE]

               if (any (x@data[, cols, drop = FALSE] != y@data[, cols, drop = FALSE]))
                 stop ("hyperSpec objects must have the same data in columns",
                       "of the same name (except data$spc)")
             }

             ## for the spectra, multiple occurences of the same wavelength are O.K.
             x@data$spc <- cbind(x@data$spc, y@data$spc)
             .wl (x) <- c (x@wavelength, y@wavelength)

             ## cbind columns in y that are not in x
             cols <- is.na (match (colnames (y@data), colnames (x@data)))
             x@data <- cbind (x@data,
                              y@data[, cols, drop = FALSE])

             .logentry (x, short = "cbind2", long = as.character (y))
           }
           )

setMethod("cbind2", signature (x = "hyperSpec", y = "missing"), function (x, y) x)
