###-----------------------------------------------------------------------------
###
### mean_sd
###
###

mean_sd <- function (x, na.rm = TRUE)
  c(mean = mean (x, na.rm = na.rm),
    sd = sd (x, na.rm = na.rm)
    )

