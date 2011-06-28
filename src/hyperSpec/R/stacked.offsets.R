###-----------------------------------------------------------------------------
###
###  stacked.offsets
###



##’ y Offsets for Stacked Plots
##’ Calculate approriate \code{yoffset} values for stacking in
##’ \code{\link[hyperSpec]{plotspc}}.
##’ 
##’ Usually, the \code{stacked} argument of \code{\link[hyperSpec]{plotspc}}
##’ will do fine, but if you need fine control over the stacking, you may
##’ calculate the y offsets yourself.
##’ 
##’ @param x a \code{hyperSpec} object
##’ @param stacked \code{TRUE} to stack single spectra.  A numeric or factor is
##’   interpreted as giving the grouping, a character is interpreted as the
##’   name of the extra data column of \code{x} that holds the grouping.
##’ @param min.zero if \code{TRUE}, the lesser of zero and the minimum
##’   intensity of the spectrum is used as minimum.
##’ @param add.factor,add.sum proportion and absolute amount of space that
##’   should be added.
##’ @param .spc for internal use. If given, the ranges are evaluated on
##’   \code{.spc}. However, this may change in future.
##’ @return a list containing \item{offsets}{numeric with the yoffset for each
##’   group in \code{stacked}} \item{groups}{numeric with the group number for
##’   each spectrum} \item{levels}{if \code{stacked} is a factor, the levels of
##’   the groups}
##’ @author C. Beleites
##’ @seealso \code{\link[hyperSpec]{plotspc}}
##’ @examples
##’ 
##’ mean.pm.sd <- aggregate (chondro, chondro$clusters, mean_pm_sd)
##’ 
##’ offset <- stacked.offsets (mean.pm.sd, ".aggregate")
##’ plot (mean.pm.sd, fill.col = matlab.palette (3), fill = ".aggregate",
##’       stacked = ".aggregate")
##’  	
##’ plot (aggregate (chondro, chondro$clusters, mean), yoffset = offset$offsets,
##’       lines.args = list (lty = 2, lwd = 2), add = TRUE)
##’ 
##’ barb <- do.call (collapse, barbiturates [1:3])
##’ plot (barb, lines.args = list (type = "h"), stacked = TRUE,
##’       stacked.args = list (add.factor = .2))
##’ 
##’ 
stacked.offsets <- function (x, stacked = TRUE,
                             min.zero = FALSE, add.factor = 0.05, add.sum = 0,
                             #tight = FALSE, TODO
                             .spc = NULL){
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

  ## should the minimum be at zero (or less)?
  if (min.zero)
    offset [1, ] <- sapply (offset [1, ], min, 0, na.rm = TRUE)

  offset [2,] <- offset[2,] - offset [1,]

  ## add some extra space
  offset [2,] <- offset [2, ] *  (1 + add.factor) + add.sum
  
  offset <- c(-offset[1,], 0) + c (0, cumsum (offset[2,]))
  
  list (offsets = offset [seq_along (groups)],
        groups = stacked,
        levels = if (is.null (lvl)) stacked else lvl
        )
}
