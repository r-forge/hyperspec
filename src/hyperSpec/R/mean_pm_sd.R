###-----------------------------------------------------------------------------
###
### mean_pm_sd
###
###

mean_pm_sd <- function (x, na.rm = TRUE){
  m <- mean (x, na.rm = na.rm)
  s <- sd (x, na.rm = na.rm)
  c("mean.minus.sd" = m - s, "mean" = m, "mean.plus.sd" = m + s)
}

