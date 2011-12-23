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
##' @param drop,na.rm,... further parameters to the \link[matrixStats]{matrixStats-package} function
##' @rdname matrixStats
##' @name matrixStats
NULL
 
##' @noRd
setGeneric ('colMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMeans) <- function (){
   colMeans (chondro)
}

##' @noRd
setGeneric ('colSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colSums) <- function (){
   colSums (chondro)
}

##' @noRd
setGeneric ('rowMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMeans) <- function (){
   rowMeans (chondro)
}

##' @noRd
setGeneric ('rowSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowSums) <- function (){
   rowSums (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("anyMissing", signature = signature (x = "hyperSpec"), function (x, ...){
                 anyMissing (x@data$spc, ...)
               }) 

.test (anyMissing) <- function (){
   anyMissing (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colAlls", signature = signature (x = "hyperSpec"), function (x, ...){
                 colAlls (x@data$spc, ...)
               }) 

.test (colAlls) <- function (){
   colAlls (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colAnys", signature = signature (x = "hyperSpec"), function (x, ...){
                 colAnys (x@data$spc, ...)
               }) 

.test (colAnys) <- function (){
   colAnys (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowAlls", signature = signature (x = "hyperSpec"), function (x, ...){
                 rowAlls (x@data$spc, ...)
               }) 

.test (rowAlls) <- function (){
   rowAlls (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowAnys", signature = signature (x = "hyperSpec"), function (x, ...){
                 rowAnys (x@data$spc, ...)
               }) 

.test (rowAnys) <- function (){
   rowAnys (chondro)
}

##' @noRd
setGeneric ('colMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMeans", date = NULL){
   result <- colMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMeans) <- function (){
   colMeans (chondro)
}

##' @noRd
setGeneric ('colSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colSums", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSums", date = NULL){
   result <- colSums (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colSums) <- function (){
   colSums (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colCollapse", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colCollapse", date = NULL){
   result <- colCollapse (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colCollapse) <- function (){
   colCollapse (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colCounts", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colCounts", date = NULL){
   result <- colCounts (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colCounts) <- function (){
   colCounts (chondro)
}

##' @noRd
setGeneric ('colDiffs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colDiffs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colDiffs", date = NULL){
   result <- colDiffs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colDiffs) <- function (){
   colDiffs (chondro)
}

##' @noRd
setGeneric ('colIQRs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colIQRs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colIQRs", date = NULL){
   result <- colIQRs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colIQRs) <- function (){
   colIQRs (chondro)
}

##' @noRd
setGeneric ('colMads', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMads", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMads", date = NULL){
   result <- colMads (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMads) <- function (){
   colMads (chondro)
}

##' @noRd
setGeneric ('colMaxs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMaxs", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMaxs", date = NULL){
   result <- colMaxs (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMaxs) <- function (){
   colMaxs (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colMedians", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMedians", date = NULL){
   result <- colMedians (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMedians) <- function (){
   colMedians (chondro)
}

##' @noRd
setGeneric ('colMins', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colMins", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colMins", date = NULL){
   result <- colMins (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colMins) <- function (){
   colMins (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colOrderStats", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colOrderStats", date = NULL){
   result <- colOrderStats (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colOrderStats) <- function (){
   colOrderStats (chondro)
}

##' @noRd
setGeneric ('colProds', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colProds", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colProds", date = NULL){
   result <- colProds (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colProds) <- function (){
   colProds (chondro)
}

##' @noRd
setGeneric ('colQuantiles', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colQuantiles", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colQuantiles", date = NULL){
   result <- colQuantiles (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colQuantiles) <- function (){
   colQuantiles (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colRanges", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colRanges", date = NULL){
   result <- colRanges (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colRanges) <- function (){
   colRanges (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colRanks", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colRanks", date = NULL){
   result <- colRanks (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colRanks) <- function (){
   colRanks (chondro)
}

##' @noRd
setGeneric ('colSds', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colSds", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colSds", date = NULL){
   result <- colSds (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colSds) <- function (){
   colSds (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("colTabulates", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colTabulates", date = NULL){
   result <- colTabulates (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colTabulates) <- function (){
   colTabulates (chondro)
}

##' @noRd
setGeneric ('colVars', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("colVars", signature = signature (x = "hyperSpec"), function (x, ..., label.spc, 
         user = NULL, short = "colVars", date = NULL){
   result <- colVars (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}) 

.test (colVars) <- function (){
   colVars (chondro)
}

##' @rdname matrixStats
##' @export
 colWeightedMeans.hyperSpec <- function (x, ..., label.spc, 
         user = NULL, short = "colWeightedMeans", date = NULL){
   result <- colWeightedMeans (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
} 

.test (colWeightedMeans.hyperSpec) <- function (){
   colWeightedMeans (chondro)
}

##' @rdname matrixStats
##' @export
 colWeightedMedians.hyperSpec <- function (x, ..., label.spc, 
         user = NULL, short = "colWeightedMedians", date = NULL){
   result <- colWeightedMedians (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
} 

.test (colWeightedMedians.hyperSpec) <- function (){
   colWeightedMedians (chondro)
}

##' @noRd
setGeneric ('rowMeans', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMeans", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMeans", date = NULL){
   result <- rowMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMeans) <- function (){
   rowMeans (chondro)
}

##' @noRd
setGeneric ('rowSums', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowSums", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSums", date = NULL){
   result <- rowSums (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowSums) <- function (){
   rowSums (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowCollapse", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowCollapse", date = NULL){
   result <- rowCollapse (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowCollapse) <- function (){
   rowCollapse (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowCounts", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowCounts", date = NULL){
   result <- rowCounts (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowCounts) <- function (){
   rowCounts (chondro)
}

##' @noRd
setGeneric ('rowDiffs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowDiffs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowDiffs", date = NULL){
   result <- rowDiffs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowDiffs) <- function (){
   rowDiffs (chondro)
}

##' @noRd
setGeneric ('rowIQRs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowIQRs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowIQRs", date = NULL){
   result <- rowIQRs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowIQRs) <- function (){
   rowIQRs (chondro)
}

##' @noRd
setGeneric ('rowMads', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMads", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMads", date = NULL){
   result <- rowMads (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMads) <- function (){
   rowMads (chondro)
}

##' @noRd
setGeneric ('rowMaxs', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMaxs", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMaxs", date = NULL){
   result <- rowMaxs (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMaxs) <- function (){
   rowMaxs (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowMedians", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMedians", date = NULL){
   result <- rowMedians (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMedians) <- function (){
   rowMedians (chondro)
}

##' @noRd
setGeneric ('rowMins', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowMins", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowMins", date = NULL){
   result <- rowMins (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowMins) <- function (){
   rowMins (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowOrderStats", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowOrderStats", date = NULL){
   result <- rowOrderStats (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowOrderStats) <- function (){
   rowOrderStats (chondro)
}

##' @noRd
setGeneric ('rowProds', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowProds", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowProds", date = NULL){
   result <- rowProds (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowProds) <- function (){
   rowProds (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowRanges", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowRanges", date = NULL){
   result <- rowRanges (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowRanges) <- function (){
   rowRanges (chondro)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowRanks", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowRanks", date = NULL){
   result <- rowRanks (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowRanks) <- function (){
   rowRanks (chondro)
}

##' @noRd
setGeneric ('rowSds', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowSds", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowSds", date = NULL){
   result <- rowSds (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowSds) <- function (){
   rowSds (chondro)
}

##' @noRd
setGeneric ('rowVars', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowVars", signature = signature (x = "hyperSpec"), function (x, ..., label.wavelength,
          user = NULL, short = "rowVars", date = NULL){
   result <- rowVars (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 

.test (rowVars) <- function (){
   rowVars (chondro)
}

##' @rdname matrixStats
##' @export
 rowWeightedMeans.hyperSpec <- function (x, ..., label.wavelength,
          user = NULL, short = "rowWeightedMeans", date = NULL){
   result <- rowWeightedMeans (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
} 

.test (rowWeightedMeans.hyperSpec) <- function (){
   rowWeightedMeans (chondro)
}

##' @rdname matrixStats
##' @export
 rowWeightedMedians.hyperSpec <- function (x, ..., label.wavelength,
          user = NULL, short = "rowWeightedMedians", date = NULL){
   result <- rowWeightedMedians (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
} 

.test (rowWeightedMedians.hyperSpec) <- function (){
   rowWeightedMedians (chondro)
}

