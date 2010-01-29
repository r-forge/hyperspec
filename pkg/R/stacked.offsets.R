###-----------------------------------------------------------------------------
###
###  stacked.offsets
###

stacked.offsets <- function (x, stacked = TRUE, .spc = NULL){
  lvl <- NULL

  if (is.character (stacked))
    stacked <- unlist (x [[, stacked]])
  else if (isTRUE (stacked))
    stacked <- row.seq (x)

  if (is.factor (stacked)) {
    lvl <- levels (stacked)
    stacked <- as.numeric (stacked)
  } else if (!is.numeric (stacked))
    stop ("stacked must be either TRUE, the name of the extra data column to use for grouping, a factor or a numeric.")

  if (is.null (.spc))
    .spc <- x@data$spc

  ## using ave would be easier, but it splits the data possibly leading to huge lists.
  groups <- unique (as.numeric (stacked))
  offset <- matrix (nrow = 2, ncol = length (groups))
  
  for (i in seq_along (groups))
    offset[, i] <- range (.spc [stacked == groups [i], ], na.rm = TRUE)

  offset [2,] <- offset[2,] - offset [1,]
  offset <- c(-offset[1,], 0) + c (0, cumsum (offset[2,]))
  
  list (offsets = offset [seq_along (groups)],
        groups = stacked,
        levels = if (is.null (lvl)) stacked else lvl
        )
}
