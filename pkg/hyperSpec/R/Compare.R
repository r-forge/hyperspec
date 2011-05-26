###-----------------------------------------------------------------------------
###
###  Compare functions
###
###

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "hyperSpec"),
           function (e1, e2){
             validObject (e1)
             validObject (e2)

             callGeneric (e1[[]], e2[[]])
           }
           )

.compx <- function (e1, e2){
  validObject (e1)
  callGeneric (e1 [[]], e2)
}

.compy <- function (e1, e2){
  validObject (e2)
  callGeneric (e1, e2 [[]])
}

setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "numeric"), .compx)
setMethod ("Compare", signature (e1 = "hyperSpec", e2 = "matrix"), .compx)

setMethod ("Compare", signature (e1 = "numeric", e2 = "hyperSpec"), .compy)
setMethod ("Compare", signature (e1 = "matrix", e2 = "hyperSpec"), .compy)


