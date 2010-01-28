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

