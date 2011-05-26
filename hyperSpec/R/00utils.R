`examples<-` <- function (f, value) {
  attr (f, "ex") <- value
  f
}

if (!require (svUnit))
  `test<-` <- function (f, value) {
    attr (f, "test") <- value
    f
  }
