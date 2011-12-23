
####################################################################################################
.funcs <- structure(list(f = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 11L, 12L,
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
  4L, 4L, 4L, 4L, 4L, 2L, 4L, 4L, 4L, 4L, 4L, 4L), .Label = c("directresult", "exclude", "loadings",
  "scores" ), class = "factor"), s3 = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
  FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
  FALSE, FALSE, TRUE, TRUE)), .Names = c("f", "type", "s3"), row.names = c(NA, 54L), class =
  "data.frame")

# excludes:
# rowQuantile - own function in rowQuantile.R
# *Tabulates - don't make sense for hyperSpec objects
# *Diff - don't make sense for hyperSpec objects

.make.matrixStats <- function (file = "matrixStats.R"){
  file <- file (file, "w")
  on.exit (close (file))
  
  cat (file = file, 
"##' matrixStats functions for hyperSpec objects
##'
##' hyperSpec objects can use matrix functions from package \\link[matrixStats]{matrixStats-package}
##' in addition to the base functions \\code{\\link[base]{colMeans}}, \\code{\\link[base]{colSums}},
##' \\code{\\link[base]{rowMeans}} and \\code{\\link[base]{rowSums}}.
##'
##' @param x hyperSpec object
##' @param label.spc labels for the intensity axis for loadings-like statistics
##' @param label.wavelength labels for the wavelength axis for scores-like statistics
##' @param user,short,date handed to \\code{\\link[hyperSpec]{logentry}}
##' @param ... further parameters to the \\link[matrixStats]{matrixStats-package} function
##'  
")
  
  for (f in which (.funcs$type != "exclude")){
    def <- switch (as.character (.funcs$type [f]),
                   directresult =  sprintf ('function (x, ...){
                 %s (x@data$spc, ...)
               }', .funcs$f [f], .funcs$f [f]),
                   loadings =  sprintf ('function (x, ..., label.spc, 
         user = NULL, short = "%s", date = NULL){
   result <- %s (x@data$spc, ...)
   if (is.matrix (result) && ncol (result) != nwl (x) && nrow (result) == nwl (x))
      result <- t (result)

   decomposition (x, result, scores = FALSE, label.spc = label.spc, 
                  user = user, short = short, date = date)
}', .funcs$f [f], .funcs$f [f]),

                   scores = sprintf ('function (x, ..., label.wavelength,
          user = NULL, short = "%s", date = NULL){
   result <- %s (x@data$spc, ...)
   if (is.matrix (result) && nrow (result) != nrow (x) && ncol (result) == nrow (x))
      result <- t (result)

   decomposition (x, result, scores = TRUE, label.wavelength = label.wavelength, 
                  user = user, short = short, date = date)
}', .funcs$f [f], .funcs$f [f]),

                   stop ("unknown function type: ", .funcs$type [f])
                 )
    
    if (.funcs$s3 [f]) {
      t <- sprintf ("%s.hyperSpec <- %s", .funcs$f [f], def)
    } else {
      t <- sprintf ('setMethod ("%s", signature = signature (x = "hyperSpec"), %s)', .funcs$f [f], def)
    }

    if (! isGeneric (as.character (.funcs$f [f])) && ! .funcs$s3 [f])
      cat (file = file, "##' @noRd\nsetGeneric ('", as.character (.funcs$f [f]),
           "', package = 'matrixStats')\n\n", sep = "")
    cat (file = file, "##' @rdname matrixStats\n##' @export\n", t, "\n")

    ## tests
    t <- sprintf ("")
    cat (file = file, sprintf ("
.test (%s%s) <- function (){
   %s (chondro)
}

", .funcs$f [f], if (.funcs$s3 [f]) ".hyperSpec" else "", .funcs$f [f])
         )
  }
}

if (require (svUnit))
  testmatrixStatfun <- function (){
    exports <- getNamespaceExports ("matrixStats")
    exports <- gsub ("^[.]__T__([^:]*):.*$", "\\1", exports)
    exports <- gsub ("^(.*)[.][^.]+$", "\\1", exports)
    exports <- unique (exports)

    checkTrue (length (setdiff (exports, .funcs$f)) == 0, "new function in matrixStats")
 }

