###-----------------------------------------------------------------------------
###
###  .na.if.different
###
###

.na.if.different <- function (x) {
  if (length (unique (x)) > 1) NA else x[1]
}
