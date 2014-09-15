##' matrixStats functions for hyperSpec objects
##'
##' hyperSpec objects can use matrix functions from package \link[matrixStats]{matrixStats-package}
##' in addition to the base functions \code{\link[base]{colMeans}}, \code{\link[base]{colSums}},
##' \code{\link[base]{rowMeans}} and \code{\link[base]{rowSums}}.
##'
##' @param x hyperSpec object
##' @param label.spc labels for the intensity axis for loadings-like statistics
##' @param label.wavelength labels for the wavelength axis for scores-like statistics
##' @param drop,na.rm,... further parameters to the \link[matrixStats]{matrixStats-package} function
##' @rdname matrixStats
##' @name matrixStats
NULL
 
##' @rdname matrixStats
##' @export
 setMethod ("anyMissing", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE){
anyMissing (x = x@data$spc, ..., na.rm = na.rm)
}) 

.test (anyMissing) <- function (){
   anyMissing (fluNA)
}

##' @rdname matrixStats
##' @export
 colAlls.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
colAlls (x = x@data$spc, value = value, na.rm = na.rm, ...)
} 

.test (colAlls.hyperSpec) <- function (){
   colAlls (fluNA)
}

##' @rdname matrixStats
##' @export
 colAnys.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
colAnys (x = x@data$spc, value = value, na.rm = na.rm, ...)
} 

.test (colAnys.hyperSpec) <- function (){
   colAnys (fluNA)
}

##' @rdname matrixStats
##' @export
 rowAlls.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
rowAlls (x = x@data$spc, value = value, na.rm = na.rm, ...)
} 

.test (rowAlls.hyperSpec) <- function (){
   rowAlls (fluNA)
}

##' @rdname matrixStats
##' @export
 rowAnys.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
rowAnys (x = x@data$spc, value = value, na.rm = na.rm, ...)
} 

.test (rowAnys.hyperSpec) <- function (){
   rowAnys (fluNA)
}

##' @rdname matrixStats
##' @export
 colCollapse.hyperSpec <- function (x, idxs, ..., na.rm = TRUE, label.spc){

   result <- colCollapse (x = x@data$spc, idxs = idxs, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

} 

.test (colCollapse.hyperSpec) <- function (){
   colCollapse (fluNA)
}

##' @rdname matrixStats
##' @export
 colCounts.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ..., label.spc){

   result <- colCounts (x = x@data$spc, value = value, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

} 

.test (colCounts.hyperSpec) <- function (){
   colCounts (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colDiffs", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.spc){

   result <- colDiffs (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colDiffs) <- function (){
   colDiffs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colIQRs", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.spc){

   result <- colIQRs (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colIQRs) <- function (){
   colIQRs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colMads", signature = signature (x = "hyperSpec"), function (x, centers = colMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.spc){

   result <- colMads (x = x@data$spc, centers = centers, constant = constant, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colMads) <- function (){
   colMads (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colMaxs", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc){

   result <- colMaxs (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colMaxs) <- function (){
   colMaxs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colMedians", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc){

   result <- colMedians (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colMedians) <- function (){
   colMedians (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colMins", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc){

   result <- colMins (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colMins) <- function (){
   colMins (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colOrderStats", signature = signature (x = "hyperSpec"), function (x, which, ..., na.rm = TRUE, label.spc){

   result <- colOrderStats (x = x@data$spc, which = which, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colOrderStats) <- function (){
   colOrderStats (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colProds", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, method = c("expSumLog", "direct"), ..., label.spc){

   result <- colProds (x = x@data$spc, na.rm = na.rm, method = method, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colProds) <- function (){
   colProds (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colQuantiles", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.spc){

   result <- colQuantiles (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colQuantiles) <- function (){
   colQuantiles (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colRanges", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.spc){

   result <- colRanges (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colRanges) <- function (){
   colRanges (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colRanks", signature = signature (x = "hyperSpec"), function (x, ties.method = c("max", "average", "min"), preserveShape = FALSE, ..., na.rm = TRUE, label.spc){

   result <- colRanks (x = x@data$spc, ties.method = ties.method, preserveShape = preserveShape, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colRanks) <- function (){
   colRanks (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colSds", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.spc){

   result <- colSds (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colSds) <- function (){
   colSds (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("colVars", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, center = NULL, ..., label.spc){

   result <- colVars (x = x@data$spc, na.rm = na.rm, center = center, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

}) 

.test (colVars) <- function (){
   colVars (fluNA)
}

##' @rdname matrixStats
##' @export
 colWeightedMeans.hyperSpec <- function (x, w = NULL, na.rm = TRUE, ..., label.spc){

   result <- colWeightedMeans (x = x@data$spc, w = w, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

} 

.test (colWeightedMeans.hyperSpec) <- function (){
   colWeightedMeans (fluNA)
}

##' @rdname matrixStats
##' @export
 colWeightedMedians.hyperSpec <- function (x, w = NULL, na.rm = TRUE, ..., label.spc){

   result <- colWeightedMedians (x = x@data$spc, w = w, na.rm = na.rm, ...)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

} 

.test (colWeightedMedians.hyperSpec) <- function (){
   colWeightedMedians (fluNA)
}

##' @rdname matrixStats
##' @export
 rowCollapse.hyperSpec <- function (x, idxs, ..., na.rm = TRUE, label.wavelength){

   result <- rowCollapse (x = x@data$spc, idxs = idxs, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

} 

.test (rowCollapse.hyperSpec) <- function (){
   rowCollapse (fluNA)
}

##' @rdname matrixStats
##' @export
 rowCounts.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ..., label.wavelength){

   result <- rowCounts (x = x@data$spc, value = value, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

} 

.test (rowCounts.hyperSpec) <- function (){
   rowCounts (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowDiffs", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.wavelength){

   result <- rowDiffs (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowDiffs) <- function (){
   rowDiffs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowIQRs", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.wavelength){

   result <- rowIQRs (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowIQRs) <- function (){
   rowIQRs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowMads", signature = signature (x = "hyperSpec"), function (x, centers = rowMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.wavelength){

   result <- rowMads (x = x@data$spc, centers = centers, constant = constant, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowMads) <- function (){
   rowMads (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowMaxs", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength){

   result <- rowMaxs (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowMaxs) <- function (){
   rowMaxs (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowMedians", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength){

   result <- rowMedians (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowMedians) <- function (){
   rowMedians (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowMins", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength){

   result <- rowMins (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowMins) <- function (){
   rowMins (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowOrderStats", signature = signature (x = "hyperSpec"), function (x, which, ..., na.rm = TRUE, label.wavelength){

   result <- rowOrderStats (x = x@data$spc, which = which, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowOrderStats) <- function (){
   rowOrderStats (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowProds", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, method = c("expSumLog", "direct"), ..., label.wavelength){

   result <- rowProds (x = x@data$spc, na.rm = na.rm, method = method, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowProds) <- function (){
   rowProds (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowQuantiles", signature = signature (x = "hyperSpec"), function (x, probs = seq(from = 0, to = 1, by = 0.25), ..., na.rm = TRUE, label.wavelength, drop = TRUE){

   result <- rowQuantiles (x = x@data$spc, probs = probs, ..., na.rm = na.rm, drop = drop)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowQuantiles) <- function (){
   rowQuantiles (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowRanges", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, ..., label.wavelength){

   result <- rowRanges (x = x@data$spc, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowRanges) <- function (){
   rowRanges (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowRanks", signature = signature (x = "hyperSpec"), function (x, ties.method = c("max", "average", "min"), ..., na.rm = TRUE, label.wavelength){

   result <- rowRanks (x = x@data$spc, ties.method = ties.method, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowRanks) <- function (){
   rowRanks (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowSds", signature = signature (x = "hyperSpec"), function (x, ..., na.rm = TRUE, label.wavelength){

   result <- rowSds (x = x@data$spc, ..., na.rm = na.rm)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowSds) <- function (){
   rowSds (fluNA)
}

##' @rdname matrixStats
##' @export
 setMethod ("rowVars", signature = signature (x = "hyperSpec"), function (x, na.rm = TRUE, center = NULL, ..., label.wavelength){

   result <- rowVars (x = x@data$spc, na.rm = na.rm, center = center, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

}) 

.test (rowVars) <- function (){
   rowVars (fluNA)
}

##' @rdname matrixStats
##' @export
 rowWeightedMeans.hyperSpec <- function (x, w = NULL, na.rm = TRUE, ..., label.wavelength){

   result <- rowWeightedMeans (x = x@data$spc, w = w, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

} 

.test (rowWeightedMeans.hyperSpec) <- function (){
   rowWeightedMeans (fluNA)
}

##' @rdname matrixStats
##' @export
 rowWeightedMedians.hyperSpec <- function (x, w = NULL, na.rm = TRUE, ..., label.wavelength){

   result <- rowWeightedMedians (x = x@data$spc, w = w, na.rm = na.rm, ...)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

} 

.test (rowWeightedMedians.hyperSpec) <- function (){
   rowWeightedMedians (fluNA)
}

##' @rdname matrixStats
##' @export
 rowAvgsPerColSet.hyperSpec <- function (X, W = NULL, S, FUN = rowMeans, ..., na.rm = TRUE, label.wavelength, tFUN = FALSE){

   result <- rowAvgsPerColSet (X = X, W = W, S = S, FUN = FUN, ..., na.rm = na.rm, tFUN = tFUN, x = x@data$spc)

   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
         result <- t (result)
         
         decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)

} 

.test (rowAvgsPerColSet.hyperSpec) <- function (){
   rowAvgsPerColSet (fluNA)
}

##' @rdname matrixStats
##' @export
 colAvgsPerRowSet.hyperSpec <- function (X, W = NULL, S, FUN = colMeans, tFUN = FALSE, ..., na.rm = TRUE, label.spc){

   result <- colAvgsPerRowSet (X = X, W = W, S = S, FUN = FUN, tFUN = tFUN, ..., na.rm = na.rm, x = x@data$spc)

   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
       result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc)

} 

.test (colAvgsPerRowSet.hyperSpec) <- function (){
   colAvgsPerRowSet (fluNA)
}

