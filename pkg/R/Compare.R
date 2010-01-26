###-----------------------------------------------------------------------------
###
###  compare functions
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


###-----------------------------------------------------------------------------
###
###  all.equal
###
###

setMethod ("all.equal", signature (target = "hyperSpec", current = "hyperSpec"),
           function (target, current, check.attributes = FALSE, check.names = FALSE, ...,
                     check.column.order = FALSE, check.label = FALSE, check.log = FALSE){
             validObject (target)
             validObject (current)

             result <- character (0)

             cmp <- all.equal (target@wavelength, current@wavelength, check.attributes, check.names, ...)
             if (! isTRUE (cmp)) result <- c("@wavelength:", cmp)

             if (check.column.order)
               cmp <- all.equal (target@data, current@data, check.attributes, ...)
             else
               cmp <- all.equal ( target@data[order (colnames ( target@data))],
                                 current@data[order (colnames (current@data))], check.attributes, check.names, ...)
             if (! isTRUE (cmp)) result <- c (result, "@data:", cmp)

             if (check.label){
               cmp <- all.equal (target@label[order (names (target@label))],
                                 current@label[order (names (current@label))], check.attributes, check.names, ...)
               if (! isTRUE (cmp)) result <- c (result, "@label:", cmp)
             }

             if (check.log) {
               cmp <- all.equal (target@log, current@log, check.attributes, check.names, ...)
               if (! isTRUE (cmp)) result <- c (result, "@log:", cmp)
             }

             if (length (result) == 0)
               TRUE
             else
               result
           }
           )

