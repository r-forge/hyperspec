###-----------------------------------------------------------------------------
###
###  is.na
###
###

setMethod ("is.na", signature (x = "hyperSpec"),
           function (x) {
             is.na (x@data$spc)
           })
           

