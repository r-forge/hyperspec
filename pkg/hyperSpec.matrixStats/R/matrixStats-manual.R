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



# test(colCollapse)   **ERROR**      0 2014-05-31 22:58:51         
# test(rowCollapse)   **ERROR**      0 2014-05-31 22:58:51         
# test(colTabulates)  **ERROR**      0 2014-05-31 22:58:51         

