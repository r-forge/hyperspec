###-----------------------------------------------------------------------------
###
###  plotc - plot timeseries, concentration, ... 
###
###  C. Beleites

plotc <- function (object, model = spc ~ c, groups = NULL,
                     func = NULL, func.args = list (), ...){
  chk.hy (object)
  validObject (object)

  dots <- list (...)

  if (! is.null (func)) 
    object <- do.call (apply, c (list (object, 1, func), func.args))
  
  ## allow to plot against the row number
  object$.row <- row.seq (object)

  groups <- substitute (groups)
  
  ## find out whether the wavelengths are needed individually,
  ## if not, use only the first wavelength and issue a warning
  parsed.formula <- latticeParseFormula (model,
        as.long.df (object [1, , 1, wl.index = TRUE], rownames = TRUE),
        groups = groups, dimension = 2)
  
  use.c <- parsed.formula$right.name
  use.spc <- parsed.formula$left.name

  if (use.spc == "spc" && nwl (object) > 1 && is.null (func) &&
      !any (grepl (".wavelength", c(as.character (model),
                                    as.character (groups),
                                    as.character (dots$subset))))) {
    object <- object [,, 1, wl.index = TRUE]
    warning ("Intensity at first wavelengh only is used.")
  }

  if (is.null (func))
    ylab <- object@label [[use.spc]]
  else {
    ylab <- substitute (func ())
    ylab [[2]] <- object@label [[use.spc]][[1]]
    for (i in seq_along (func.args)){
      if (names (func.args)[[i]] == "")
        ylab [[i + 2]] <- func.args [[i]]
      else
        ylab [[i + 2]] <- bquote (.(x) == .(y),
                                  list (x = names (func.args) [[i]],
                                        y = as.character (func.args [[i]])))
      
      }
    ylab <- as.expression (ylab)
  }
  
  ## set defaults: axis labels, plot style
  dots <- modifyList (list (xlab = object@label [[use.c]],
                            ylab = ylab,
                            pch = 19),
                      dots)

  ## expand the data.frame
  df <- as.long.df (object, rownames = TRUE, wl.factor = TRUE)

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

