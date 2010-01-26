#################################################################################
###
###  sample.R - sample Method for hyperSpec objects
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-01-26 at 17:49:46 on cb>
###  
###  generates random selection of spectra
###  
###  Version 1.0  2010-01-25 16:46  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

setGeneric ("sample",
            def = function (x, size, replace = FALSE, prob = NULL, ...){
              standardGeneric ("sample")
            },
            useAsDefault = function (x, size, replace = FALSE, prob = NULL, ...){
              base::sample (x = x, size = size, replace = replace, prob = prob, ...)
            }
)

setMethod ("sample", signature = "hyperSpec",
           function (x, size, replace = FALSE, prob = NULL, index = FALSE, ...,
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





