###-----------------------------------------------------------------------------
###
### mean_sd
###
###

mean_sd <- function (x, na.rm = TRUE)
  c(mean (x, na.rm = na.rm),  sd (x, na.rm = na.rm))

