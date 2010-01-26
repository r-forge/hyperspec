#################################################################################
###
###  plotc.R - plot calibration graph, time series, etc.
###  Time-stamp: <Claudia Beleites on Tuesday, 2010-01-26 at 17:31:32 on cb>
###  
###  
###  Version 1.0  2010-01-12 13:58  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

plotc <- function (object, model = spc ~ c, groups = NULL,
                     func = sum, func.args = list (), ...){
  .is.hy (object)
  validObject (object)

  object$.row <- seq_len (nrow (object))

  groups <- substitute (groups)
  
  dots <- list (...)

  parsed.formula <- latticeParseFormula (model,
        as.long.df (object [1, , 1, wl.index = TRUE], rownames = TRUE),
        groups = groups, dimension = 2)
  
  use.c <- parsed.formula$right.name
  use.spc <- parsed.formula$left.name

  if (use.spc == "spc" && nwl (object) > 1 &&
      !any (grepl (".wavelength", c(as.character (model),
                                    as.character (groups),
                                    as.character (dots$subset))))) {
    object <- do.call (apply, c (list (object, 1, func), func.args))
  }
  
  dots <- modifyList (list (xlab = object@label [[use.c]],
                            ylab = object@label [[use.spc]],
                            pch = 19),
                      dots)

  df <- as.long.df (object, rownames = TRUE)

  if ((! is.null (parsed.formula$condition) &&
       parsed.formula$condition == ".wavelength") ||
      (! is.null (groups) &&
       as.character (groups) == ".wavelength"))
    df$.wavelength <- as.factor (df$.wavelength)
 
  do.call(xyplot, c (list (x = model, data = df, groups = groups), dots))
}

###-----------------------------------------------------------------------------
###
###  plotc,old - plot timeseries, concentration, ... old version
###
plotc.old <- function (object, use.c = "c", func = sum, ...,
                   z = NULL, zlab = NULL, add = FALSE,
                   plot.args = list()) {

  .is.hy (object)
  validObject (object)

  ic <- pmatch (use.c, colnames (object@data))
  if (is.null (ic))
    stop (paste ("hyperSpec object has no column ", use.c))

  if (!is.null (z)){
    if (is.numeric (z)){
      z <- rep (z, length.out = nrow (object))
      if (is.null (zlab))
        zlab = "z"
    } else if (is.character (z)){
      if (is.na (match (z, colnames (object@data))))
        stop ("z did not evaluate to a column in object@data.")
      if (is.null (zlab))
        zlab <- object@label[[z]]
      if (is.null (zlab))
        zlab <- z
      z <- object@data[, z]
    }
  } else {
    z <- apply (object [[]], 1, func, ...)

    if (is.null (zlab)){
      zlab <- as.expression (substitute (func))
      if (nwl (object) == 1 & as.character(zlab) == "sum")
        zlab <- object@label$spc
      else
        zlab <-  paste (zlab, object@label$spc)
    }
  }

  plot.args <- c(list (x = unlist (object[[, ic]]),
                       y  = as.numeric (z)),
                 plot.args)

  if (is.null (plot.args$xlab)){
    plot.args$xlab <- object@label[[names(object@data)[ic]]]
    if (is.null (plot.args$xlab))
      plot.args$xlab <- use.c
  }

  if (is.null (plot.args$ylab)){
    plot.args$ylab <- zlab
    if (is.null (plot.args$ylab))
      plot.args$ylab <- z
  }

  if (is.null (plot.args$pch))
    plot.args$pch = 20

  if (is.null(plot.args$type))
    plot.args$type = "p"

  if (add)
    do.call(lines, plot.args)
  else
    do.call(plot, plot.args)
}

