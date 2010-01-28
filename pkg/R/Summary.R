###-----------------------------------------------------------------------------
###
###  summary functions
###
###

setMethod ("Summary", signature (x = "hyperSpec"),
           function (x, ..., na.rm = FALSE){
             validObject (x)

             if ((.Generic == "prod") || (.Generic == "sum"))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))

             ## dispatch also on the objects in ...
             x <- sapply (list (x[[]], ...), .Generic, na.rm = na.rm)

             callGeneric (x, na.rm = na.rm)
           }
           )

