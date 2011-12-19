
##' matrixStats functions for hyperSpec objects
##'
##' hyperSpec objects can use matrix functions from package \link[matrixStats]{matrixStats-package}
##' in addition to the base functions \code{\link[base]{colMeans}}, \code{\link[base]{colSums}},
##' \code{\link[base]{rowMeans}} and \code{\link[base]{rowSums}}.
##'
##' @param x hyperSpec object
##' @param label.spc labels for the intensity axis for loadings-like statistics
##' @param label.wavelength labels for the wavelength axis for scores-like statistics
##' @param user,short,date handed to \code{\link[hyperSpec]{logentry}}
##' @param ... further parameters to the \link[matrixStats]{matrixStats-package} function
##'  
##' @rdname matrixStats
setGeneric ('colMeans', package = 'base')

##' @rdname matrixStats
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colSums', package = 'base')

##' @rdname matrixStats
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowMeans', package = 'base')

##' @rdname matrixStats
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowSums', package = 'base')

##' @rdname matrixStats
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("anyMissing", signature = signature (x = "hyperSpec"), function (x, ...){
                 anyMissing (x@data$spc, ...)
               }) 

##' @rdname matrixStats
 setMethod ("colAlls", signature = signature (x = "hyperSpec"), function (x, ...){
                 colAlls (x@data$spc, ...)
               }) 

##' @rdname matrixStats
 setMethod ("colAnys", signature = signature (x = "hyperSpec"), function (x, ...){
                 colAnys (x@data$spc, ...)
               }) 

##' @rdname matrixStats
 setMethod ("rowAlls", signature = signature (x = "hyperSpec"), function (x, ...){
                 rowAlls (x@data$spc, ...)
               }) 

##' @rdname matrixStats
 setMethod ("rowAnys", signature = signature (x = "hyperSpec"), function (x, ...){
                 rowAnys (x@data$spc, ...)
               }) 

##' @noRd
setGeneric ('colMeans', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colSums', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colCollapse", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colCollapse", date = NULL){
   result <- colCollapse (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colCounts", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colCounts", date = NULL){
   result <- colCounts (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colDiffs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colDiffs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colDiffs", date = NULL){
   result <- colDiffs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colIQRs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colIQRs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colIQRs", date = NULL){
   result <- colIQRs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colMads', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colMads", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMads", date = NULL){
   result <- colMads (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colMaxs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colMaxs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMaxs", date = NULL){
   result <- colMaxs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colMedians", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMedians", date = NULL){
   result <- colMedians (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colMins', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colMins", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMins", date = NULL){
   result <- colMins (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colOrderStats", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colOrderStats", date = NULL){
   result <- colOrderStats (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colProds', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colProds", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colProds", date = NULL){
   result <- colProds (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colQuantiles', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colQuantiles", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colQuantiles", date = NULL){
   result <- colQuantiles (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colRanges", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colRanges", date = NULL){
   result <- colRanges (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colRanks", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colRanks", date = NULL){
   result <- colRanks (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colSds', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colSds", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSds", date = NULL){
   result <- colSds (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("colTabulates", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colTabulates", date = NULL){
   result <- colTabulates (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colVars', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("colVars", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colVars", date = NULL){
   result <- colVars (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('colWeightedMeans', package = 'matrixStats')

##' @rdname matrixStats
 colWeightedMeans.hyperSpec <- function (x, ..., label.spc, 
         user = NULL, short = "colWeightedMeans", date = NULL){
   result <- colWeightedMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
} 

##' @noRd
setGeneric ('colWeightedMedians', package = 'matrixStats')

##' @rdname matrixStats
 colWeightedMedians.hyperSpec <- function (x, ..., label.spc, 
         user = NULL, short = "colWeightedMedians", date = NULL){
   result <- colWeightedMedians (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
} 

##' @noRd
setGeneric ('rowMeans', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowSums', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("rowCollapse", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowCollapse", date = NULL){
   result <- rowCollapse (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("rowCounts", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowCounts", date = NULL){
   result <- rowCounts (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowDiffs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowDiffs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowDiffs", date = NULL){
   result <- rowDiffs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowIQRs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowIQRs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowIQRs", date = NULL){
   result <- rowIQRs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowMads', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowMads", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMads", date = NULL){
   result <- rowMads (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowMaxs', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowMaxs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMaxs", date = NULL){
   result <- rowMaxs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("rowMedians", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMedians", date = NULL){
   result <- rowMedians (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowMins', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowMins", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMins", date = NULL){
   result <- rowMins (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("rowOrderStats", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowOrderStats", date = NULL){
   result <- rowOrderStats (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowProds', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowProds", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowProds", date = NULL){
   result <- rowProds (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowQuantiles', package = 'matrixStats')

##' @rdname matrixStats
 rowQuantiles.hyperSpec <- function (x, ..., label.wavelength,
          user = NULL, short = "rowQuantiles", date = NULL){
   result <- rowQuantiles (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
} 

##' @rdname matrixStats
 setMethod ("rowRanges", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowRanges", date = NULL){
   result <- rowRanges (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @rdname matrixStats
 setMethod ("rowRanks", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowRanks", date = NULL){
   result <- rowRanks (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowSds', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowSds", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSds", date = NULL){
   result <- rowSds (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowVars', package = 'matrixStats')

##' @rdname matrixStats
 setMethod ("rowVars", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowVars", date = NULL){
   result <- rowVars (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

##' @noRd
setGeneric ('rowWeightedMeans', package = 'matrixStats')

##' @rdname matrixStats
 rowWeightedMeans.hyperSpec <- function (x, ..., label.wavelength,
          user = NULL, short = "rowWeightedMeans", date = NULL){
   result <- rowWeightedMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
} 

##' @noRd
setGeneric ('rowWeightedMedians', package = 'matrixStats')

##' @rdname matrixStats
 rowWeightedMedians.hyperSpec <- function (x, ..., label.wavelength,
          user = NULL, short = "rowWeightedMedians", date = NULL){
   result <- rowWeightedMedians (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
} 


####################################################################################################

.make.matrixStats <- function (){

  funcs <- structure(list(f = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 11L, 12L,
  14L, 15L, 1L, 2L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 10L, 30L,
  31L, 32L, 3L, 4L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L,
  49L), .Label = c("colMeans", "colSums", "rowMeans", "rowSums", "anyMissing", "colAlls", "colAnys",
  "rowAlls", "rowAnys", "colTabulates", "indexByRow", "madDiff", "rowTabulates", "sdDiff", "varDiff",
  "colCollapse", "colCounts", "colDiffs", "colIQRs", "colMads", "colMaxs", "colMedians", "colMins",
  "colOrderStats", "colProds", "colQuantiles", "colRanges", "colRanks", "colSds", "colVars",
  "colWeightedMeans", "colWeightedMedians", "rowCollapse", "rowCounts", "rowDiffs", "rowIQRs",
  "rowMads", "rowMaxs", "rowMedians", "rowMins", "rowOrderStats", "rowProds", "rowQuantiles",
  "rowRanges", "rowRanks", "rowSds", "rowVars", "rowWeightedMeans", "rowWeightedMedians" ), class =
  "factor"), type = structure(c(3L, 3L, 4L, 4L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
  3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
  4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("directresult", "exclude", "loadings",
  "scores" ), class = "factor"), s3 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
  FALSE, FALSE, TRUE, TRUE)), .Names = c("f", "type", "s3"), row.names = c(NA, 54L), class =
  "data.frame")

  for (f in which (funcs$type != "exclude")){
    def <- switch (as.character (funcs$type [f]),
                   directresult =  sprintf ('function (x, ...){
                 %s (x@data$spc, ...)
               }', funcs$f [f], funcs$f [f]),
                   loadings =  sprintf ('function (x, ..., label.spc, 
         user = NULL, short = "%s", date = NULL){
   result <- %s (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}', funcs$f [f], funcs$f [f]),

                   scores = sprintf ('function (x, ..., label.wavelength,
          user = NULL, short = "%s", date = NULL){
   result <- %s (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}', funcs$f [f], funcs$f [f]),

                   stop ("unknown function type: ", funcs$type [f])
                 )
    
    if (funcs$s3 [f]) {
      t <- sprintf ("%s.hyperSpec <- %s", funcs$f [f], def)
    } else {
      t <- sprintf ('setMethod ("%s", signature = signature (x = "hyperSpec"), %s)', funcs$f [f], def)
    }

    if (! isGeneric (as.character (funcs$f [f])))
      cat ("##' @noRd\nsetGeneric ('", as.character (funcs$f [f]),
           "', package = 'matrixStats')\n\n", sep = "")
    cat ("##' @rdname matrixStats\n", t, "\n\n")
  }
}

if (require (svUnit))
  testmatrixStatfun <- function (){
    checkTrue (require (matrixStats))
    exports <- getNamespaceExports ("matrixStats")
    exports <- gsub ("^[.]__T__([^:]*):.*$", "\\1", exports)
    exports <- gsub ("^(.*)[.][^.]+$", "\\1", exports)
    exports <- unique (exports)

    checkTrue (length (setdiff (exports, c(directresult, exclude, loadinglike, scoreslike))) == 0,
              "new function in matrixStats")
 }

.makeusage <- function (){
  arglist <- character (0)
  for (f in c (directresult, loadinglike, scoreslike)){
    fun <- get (f)

    formals <- formals (fun)
    arglist <- c (arglist, names (formals))
    formals <- mapply (names (formals), as.character (formals),
                       FUN = function (x, y)
                       if (y == "") x else paste (x, y, sep = " = ")
                       )
    formals <- paste (formals, collapse = ", ")
    
    cat ("##' \\S4method{", f, "}{hyperSpec}(",
         formals,
         ")\n",
         sep = "")
  }

  arglist <- unique (arglist)
  invisible (lapply (arglist, function (x) cat ("##' @param ", x, "\n", sep = "")))
}

