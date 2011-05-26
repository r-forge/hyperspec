###-----------------------------------------------------------------------------
###
###  %*%
###
###

setMethod ("%*%", signature (x = "hyperSpec", y = "hyperSpec"),
           function (x, y){
             validObject (x)
             validObject (y)

             if (ncol(y) > 1)
               warning(paste("Dropping column(s) of y:", paste(colnames(y$..),
                                                               collapse = ", ")))

             x@data$spc <-  x@data$spc %*% y@data$spc
             .wl (x) <- y@wavelength
             x@label$.wavelength = y@label$.wavelength

             .logentry (x, short = "%*%", long = as.character (y))
           }
           )

setMethod ("%*%", signature (x = "hyperSpec", y = "matrix"),
           function (x, y){
             validObject (x)
             x@data$spc <-  x@data$spc %*% y
             .wl (x) <- seq_len (ncol (y))
             x@label$.wavelength = NA
             .logentry (x, short = "%*%", long = list (y = .paste.row (y, val = TRUE)))
           }
           )

setMethod ("%*%", signature (x = "matrix", y = "hyperSpec"),
           function (x, y){
             validObject (y)

             if (ncol(y) > 1)
               warning(paste("Dropping column(s) of y:", paste(colnames(y$..),
                                                               collapse = ", ")))
             y <- new ("hyperSpec",
                       wavelength = y@wavelength,
                       spc = x %*% y@data$spc,
                       log = y@log
                       )

             .logentry (y, short = "%*%", long = list (x = .paste.row (x, val = TRUE)))
           }
           )
