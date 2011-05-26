###-----------------------------------------------------------------------------
###
###  Math2 functions
###
###

setMethod ("Math2", signature (x = "hyperSpec"),
           function (x, digits){
             validObject (x)
             
             x [[]] <- callGeneric (x[[]], digits)
             
             .logentry (x, short = .Generic,
                        long = list(if (exists ("digits")) digits = digits))
           }
           )

