##' @include hyperspec-matrixStats-package.R
##' @include matrixStats.R
.test (rowOrderStats) <- function (){
  rowOrderStats (fluNA, which = 1)
}

.test (colOrderStats) <- function (){
  colOrderStats (fluNA, which = 1)
}


setMethod ("colMads", signature = signature (x = "hyperSpec", centers = "numeric"), function (x, centers = colMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.spc){
  
  result <- colMads (x = x@data$spc, centers = centers, constant = constant, ..., na.rm = na.rm)
  
  if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
    result <- t (result)
  
  decomposition (x, result, scores = FALSE, label.spc = label.spc)
  
}) 
##' @export
setMethod ("colMads", signature = signature (x = "hyperSpec", centers = "hyperSpec"), function (x, centers = colMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.spc){
  colMads (x = x, centers = centers@data$spc, constant = constant, ..., na.rm = na.rm)
}) 
##' @export
setMethod ("colMads", signature = signature (x = "hyperSpec", centers = "missing"), function (x, centers = colMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.spc){
  colMads (x = x, centers = centers@data$spc, constant = constant, ..., na.rm = na.rm)
}) 

.test (colMads) <- function (){
  colMads (fluNA)
  colMads (fluNA, fluNA [1])
  colMads (fluNA, fluNA [[1]])
}

setMethod ("rowMads", signature = signature (x = "hyperSpec", centers = "numeric"), function (x, centers = rowMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.wavelength){

  result <- rowMads (x = x@data$spc, centers = centers, constant = constant, ..., na.rm = na.rm)
  
  if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
    result <- t (result)
  
  decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)
}) 
##' @export
setMethod ("rowMads", signature = signature (x = "hyperSpec", centers = "hyperSpec"), function (x, centers = rowMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.wavelength){
  rowMads (x = x, centers = as.numeric (centers@data$spc), constant = constant, ..., na.rm = na.rm)
}) 
##' @export
setMethod ("rowMads", signature = signature (x = "hyperSpec", centers = "missing"), function (x, centers = rowMedians(x, ...), constant = 1.4826, ..., na.rm = TRUE, label.wavelength){
  rowMads (x = x, centers = as.numeric (centers@data$spc), constant = constant, ..., na.rm = na.rm)
}) 

.test (rowMads) <- function (){
  rowMads (fluNA)
  rowMads (fluNA, as.numeric (fluNA [[,, 405]]))
  rowMads (fluNA, 1:6)
}

.test (rowCollapse) <- function (){
  rowCollapse (fluNA, 2)
  rowCollapse (fluNA, 1:6)
}

.test (colCollapse) <- function (){
  colCollapse (fluNA, 3)
  colCollapse (fluNA, 1 : 6)
}

# ##' @rdname matrixStats
# ##' @export
# colAlls.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
#   colAlls (x = x@data$spc, value = value, na.rm = na.rm, ...)
# } 
# 
# .test (colAlls.hyperSpec) <- function (){
#   colAlls (fluNA)
# }
# 
# ##' @rdname matrixStats
# ##' @export
# colAnys.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
#   colAnys (x = x@data$spc, value = value, na.rm = na.rm, ...)
# } 
# 
# .test (colAnys.hyperSpec) <- function (){
#   colAnys (fluNA)
# }
# 
# ##' @rdname matrixStats
# ##' @export
# rowAlls.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
#   rowAlls (x = x@data$spc, value = value, na.rm = na.rm, ...)
# } 
# 
# .test (rowAlls.hyperSpec) <- function (){
#   rowAlls (fluNA)
# }
# 
# ##' @rdname matrixStats
# ##' @export
# rowAnys.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ...){
#   rowAnys (x = x@data$spc, value = value, na.rm = na.rm, ...)
# } 
# 
# .test (rowAnys.hyperSpec) <- function (){
#   rowAnys (fluNA)
# }
# 
# ##' @rdname matrixStats
# ##' @export
# colCounts.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ..., label.spc){
#   
#   result <- colCounts (x = x@data$spc, value = value, na.rm = na.rm, ...)
#   
#   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
#     result <- t (result)
#   
#   decomposition (x, result, scores = FALSE, label.spc = label.spc)
#   
# } 
# 
# .test (colCounts.hyperSpec) <- function (){
#   colCounts (fluNA)
# }
# 
# ##' @rdname matrixStats
# ##' @export
# rowCounts.hyperSpec <- function (x, value = TRUE, na.rm = TRUE, ..., label.wavelength){
#   
#   result <- rowCounts (x = x@data$spc, value = value, na.rm = na.rm, ...)
#   
#   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
#     result <- t (result)
#   
#   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)
#   
# } 
# 
# .test (rowCounts.hyperSpec) <- function (){
#   rowCounts (fluNA)
# }

# ##' @rdname matrixStats
# ##' @export
# colCollapse.hyperSpec <- function (x, idxs, ..., na.rm = TRUE, label.spc){
#   
#   result <- colCollapse (x = x@data$spc, idxs = idxs, ..., na.rm = na.rm)
#   
#   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
#     result <- t (result)
#   
#   decomposition (x, result, scores = FALSE, label.spc = label.spc)
#   
# } 
# 
# .test (colCollapse.hyperSpec) <- function (){
#   colCollapse (fluNA)
# }

# ##' @rdname matrixStats
# ##' @export
# rowCollapse.hyperSpec <- function (x, idxs, ..., na.rm = TRUE, label.wavelength){
#   
#   result <- rowCollapse (x = x@data$spc, idxs = idxs, ..., na.rm = na.rm)
#   
#   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
#     result <- t (result)
#   
#   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength)
#   
# } 
# 
# .test (rowCollapse.hyperSpec) <- function (){
#   rowCollapse (fluNA)
# }
# test(colCollapse)   **ERROR**      0 2014-05-31 22:58:51         
# test(rowCollapse)   **ERROR**      0 2014-05-31 22:58:51         
# test(colTabulates)  **ERROR**      0 2014-05-31 22:58:51         

