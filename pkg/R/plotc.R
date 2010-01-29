###-----------------------------------------------------------------------------
###
###  plotc - plot timeseries, concentration, ... 
###
###  C. Beleites

plotc <- function (object, model = spc ~ c, groups = NULL,
                     func = sum, func.args = list (), ...){
  .is.hy (object)
  validObject (object)

  ## allow to plot against the row number
  object$.row <- row.seq (object)

  
  groups <- substitute (groups)
  
  dots <- list (...)

  ## find out whether the wavelengths are needed individually,
  ## if not, apply a summary statistic _before_ expanding the
  ## data.frame
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

  ## set defaults: axis labels, plot style
  dots <- modifyList (list (xlab = object@label [[use.c]],
                            ylab = object@label [[use.spc]],
                            pch = 19),
                      dots)

  ## expand the data.frame
  df <- as.long.df (object, rownames = TRUE)

  ## if plots should be grouped or conditioned by wavelength,
  ## it is better to have a factor
  if ((! is.null (parsed.formula$condition) &&
       parsed.formula$condition == ".wavelength") ||
      (! is.null (groups) &&
       as.character (groups) == ".wavelength"))
    df$.wavelength <- as.factor (df$.wavelength)

  ## plot
  do.call(xyplot, c (list (x = model, data = df, groups = groups), dots))
}

