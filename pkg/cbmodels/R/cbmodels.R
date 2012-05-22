##' Collection of mainly combined models
##'
##' This package provides a collection of models, including "combined" models:
##'
##' \itemize{
##' \item PCA-LDA
##' \item PLS-LDA
##' \item PCA with added reference
##' \item EMSC
##' }
##' 
##' @name cbmodels-package
##' @docType package
##' 
{
  if (!require (svUnit)){
    `.test<-` <- function (f, value) {
      attr (f, "test") <- value
      f
    }
  } else {
    `.test<-` <- svUnit::`test<-`
    checkEqualsOrdered <- function (target, current, ...)
      checkEquals (target [order (names (target))], current [order (names (current))], ...)

    checkEqualAttributes <- function (target, current, ...)
      checkEqualsOrdered (attributes (target), attributes (current), ...) # TODO: exclusion list

    svTest <- function (...){}
  }
}

