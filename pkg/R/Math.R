#################################################################################
###
###  Math.R - mathematical functions
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-01-26 at 17:58:57 on cb>
###  
###  Group Methods for Arith, Math, Math2,
###  and Methods for %*% and log
###  
###  Version 1.0  2010-01-26 09:38  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

###-----------------------------------------------------------------------------
###
###  Arith
###
###

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)
             if (.Generic %in% c ("*", "^", "%%", "%/%", "/"))
               warning (paste ("Do you really want to use", .Generic, "on 2 hyperSpec objects?"))
             e1 [[]] <- callGeneric (e1[[]], e2[[]])
             .logentry (e1, short = .Generic, long = as.character (e2))
           }
           )

.arithx <- function (e1, e2){
  validObject (e1)
  if (missing (e2)){
    e1  [[]] <- callGeneric (e1 [[]])
    .logentry (e1, short = .Generic, long = list ())
  } else {
    e1  [[]] <- callGeneric (e1 [[]], e2)
    .logentry (e1, short = .Generic,
               long = list (e2 = .paste.row (e2, val = TRUE)))
  }
}

.arithy <- function (e1, e2){
  validObject (e2)
  e2  [[]] <- callGeneric (e1, e2 [[]])
  .logentry (e2, short = .Generic, long = list (e1 = .paste.row (e1, val = TRUE)))
}

setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "numeric"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "matrix"), .arithx)
setMethod ("Arith", signature (e1 = "hyperSpec", e2 = "missing"), .arithx)

setMethod ("Arith", signature (e1 = "numeric", e2 = "hyperSpec"), .arithy)
setMethod ("Arith", signature (e1 = "matrix", e2 = "hyperSpec"), .arithy)

###-----------------------------------------------------------------------------
###
###  Math functions
###
###

setMethod ("Math", signature (x = "hyperSpec"),
           function (x){
             validObject (x)

             if (grepl ("^cum", .Generic) || grepl ("gamma$", .Generic))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))

             x [[]] <- callGeneric (x[[]])
             .logentry (x, short = .Generic, long = list())
           }
           )

###-----------------------------------------------------------------------------
###
###  Math2 functions
###
###

setMethod ("Math2", signature (x = "hyperSpec"),
           function (x, digits){
             validObject (x)
             
             x [[]] <- callGeneric (x[[]], digits)
             
             .logentry (x, short = .Generic,
                        long = list(if (exists ("digits")) digits = digits))
           }
           )

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




###-----------------------------------------------------------------------------
###
###  log
###
###

setMethod ("log", signature (x = "hyperSpec"),
           function (x, base = exp (1), ...,
                     short = "log", user = NULL, date = NULL){
             validObject (x)

             x [[]] <-  log (x[[]], base = base)
             .logentry (x, short = "log", long = list (base = base))
           }
           )
