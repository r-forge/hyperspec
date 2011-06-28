##' @nord
`examples<-` <- function (f, value) {
  attr (f, "ex") <- value
  f
}

##' @nord
if (!require (svUnit))
  `test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
 }

