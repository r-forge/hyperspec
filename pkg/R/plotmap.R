#################################################################################
###
###  plotmap.R - plot spectral maps
###  Time-stamp: <Claudia Beleites on Wednesday, 2009-12-16 at 15:39:51 on cb>
###  
###  plots intensity or extra data column over 2 extra data columns
###  
#################################################################################

## FIXME: one column => to be fixed in levelplot default panel function

plotmap.old <- function (object, x = "x", y = "y", z = "spc", cond = NULL,
                     func = mean, func.args = list (), ...){
  .is.hy (object)
  validObject (object)
  
  formula <- paste (z, "~", x, "*", y)
  if (! is.null (cond))
    formula <- paste (formula, "|", cond)
  formula <- as.formula (formula)
  
  data <- object@data
  if (!is.null (dim (object@data [[z]])) && ncol (object@data [[z]]) > 1)
    data [[z]] <- do.call (apply, c(list (data [[z]], 1,func), func.args))
  
  dots <- modifyList (list (xlab = object@label [[x]],
                            ylab = object@label [[y]],
                            aspect = "iso"),
                      list (...))
                       
  dots <- c (list (x = formula, data = data), dots)
                       

  ## if (is.null (trellis.args$panel)){
  ##   trellis.args$panel <- function (x, y, z, subscripts, ..., panel.bg = NA) {
  ##     tmp <- index.grid (x[subscripts], y[subscripts], z [subscripts])
  ##     panel.fill (col = panel.bg)
  ##     panel.levelplot (tmp$x, tmp$y, tmp$z, subscripts = TRUE, ...)
  ##   }
  ## }

  do.call(levelplot, dots)
}
