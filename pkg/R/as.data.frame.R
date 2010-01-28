###-----------------------------------------------------------------------------
###
### as.data.frame
###

setMethod ("as.data.frame",
           signature (x = "hyperSpec", row.names = "missing", optional = "missing"),
           function (x, ...){
             validObject (x)

             x@data
           })

