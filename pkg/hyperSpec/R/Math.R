###-----------------------------------------------------------------------------
###
###  Math functions
###
###

setMethod ("Math", signature (x = "hyperSpec"),
           function (x){
             validObject (x)

             if (grepl ("^cum", .Generic) || grepl ("gamma$", .Generic))
               warning (paste ("Do you really want to use", .Generic, "on a hyperSpec object?"))

             x [[]] <- callGeneric (x[[]])
             .logentry (x, short = .Generic, long = list())
           }
           )
