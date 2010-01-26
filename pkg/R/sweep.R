###-----------------------------------------------------------------------------
###
### sweep
###
###

setMethod ("sweep", "hyperSpec", function (x, MARGIN, STATS, FUN = "-",
                                           check.margin = TRUE, ...,
                                           short = "sweep", user = NULL, date = NULL){
  validObject (x)

  if (is (STATS, "hyperSpec")){
    validObject (STATS)
    STATS <- STATS@data$spc
  }

  x@data$spc <- do.call (sweep, c (list (x = x@data$spc,
                                         MARGIN = MARGIN,
                                         STATS = STATS,
                                         FUN = FUN,
                                         check.margin = check.margin),
                                   ...)
                         )

  .logentry (x, short = short,
             long = list (MARGIN = MARGIN, FUN = FUN, STATS = STATS,  FUN = FUN,
                          check.margin = check.margin, ...),
             date = date, user = user)
                      
})
