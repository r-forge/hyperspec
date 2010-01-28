###------------------------------------------------------------------------------
###
###  make sample generic with ... argument
###

setGeneric ("sample",
            def = function (x, size, replace = FALSE, prob = NULL, ...){
              standardGeneric ("sample")
            },
            useAsDefault = function (x, size, replace = FALSE, prob = NULL, ...){
              base::sample (x = x, size = size, replace = replace, prob = prob, ...)
            }
)

###------------------------------------------------------------------------------
###
###  sample
###

setMethod ("sample", signature = "hyperSpec",
           function (x, size, replace = FALSE, prob = NULL,  
                     index = FALSE,
                     short = "sample", user = NULL, date = NULL) {
             validObject (x)

             s <- sample (nrow (x), size = size, replace = replace, prob = prob)

             if (index)
               s
             else
               .logentry (x [s], short = short,
                          long = list (size = size, replace = replace,
                            prob = prob, index = index),
                          user = user, date = date)
           }
           )





