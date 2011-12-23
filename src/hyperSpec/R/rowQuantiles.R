##' @noRd
setGeneric ('rowQuantiles', package = 'matrixStats')

##' @rdname matrixStats
##' @export
 setMethod ("rowQuantiles", signature = signature (x = "hyperSpec"),
            function (x, ..., label.wavelength, 
                      user = NULL, short = "rowQuantiles", date = NULL,
                      drop = FALSE){

   if (drop) warning ("rowQuantiles for hyperSpec objects ignores drop = TRUE.")

   result <- rowQuantiles (x@data$spc, ..., drop = FALSE)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}) 


.test (rowQuantiles) <- function (){
   rowQuantiles (chondro)
}
