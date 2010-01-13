#################################################################################
###
###  plotc.R - plot calibration graph, time series, etc.
###  Time-stamp: <Claudia Beleites on Wednesday, 2010-01-13 at 14:14:38 on cb>
###  
###  
###  Version 1.0  2010-01-12 13:58  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

plotc <- function (object, model = spc ~ c, groups = NULL,
                     func = sum, func.args = list (), ...){
  .is.hy (object)
  validObject (object)

  groups <- substitute (groups)
  
  dots <- list (...)

  parsed.formula <- latticeParseFormula (model,
        as.long.df (object [1, , 1, wl.index = TRUE]),
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

  df <- as.long.df (object)

  if ((! is.null (parsed.formula$condition) &&
       parsed.formula$condition == ".wavelength") ||
      (! is.null (groups) &&
       as.character (groups) == ".wavelength"))
    df$.wavelength <- as.factor (df$.wavelength)
 
  do.call(xyplot, c (list (x = model, data = df, groups = groups), dots))
}
