##' matrixStats for hyperSpec objects
##' This package provides the functions defined in \code{\link[matrixStats]{matrixStats-package}}
##' for \code{\link[hyperSpec]{hyperSpec-class}} objects.
##' 
##' @name hyperSpec.matrixStats-package
##' @title Package hyperSpec.matrixStats
##' @docType package
##' @author C. Beleites
##' 
##' Maintainer: Claudia Beleites <Claudia.Beleites@@ipht-jena.de>
##' @seealso \code{citation ("hyperSpec-matrixStats")} produces the correct citation.
##' 
##' \code{package?hyperSpec.matrixStats} for information about the package
##' @rdname hyperSpec.matrixStats-package
##' @keywords package
##' @import matrixStats
##' @importClassesFrom hyperSpec hyperSpec
{
if (!require (svUnit, quietly = TRUE))
  `.test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
 } else {
 `.test<-` <- svUnit::`test<-`
 }
fluNA <- hyperSpec:::fluNA
}

