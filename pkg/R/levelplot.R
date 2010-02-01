#################################################################################
###
###  levelplot.R - everything that has to do with levelplot-like plotting:
###
###  levelplot is used by plotmap, plotmat, plotvoronoi
###

### the workhorse function
.levelplot <- function (x, data, transform.factor = TRUE, ...) {
  validObject (data)

  data$.row <- row.seq (data)

  ## parse formula to find the columns to be plotted
  ## they may include also "wavelength"
  parsed.formula <- latticeParseFormula (x,
        as.long.df (data [1, , 1, wl.index = TRUE], rownames = TRUE),
        dimension = 3)
  use.x <- parsed.formula$right.x.name
  use.y <- parsed.formula$right.y.name
  use.z <- parsed.formula$left.name

  dots <- list (...)
  
  ## if spc is used as z and the data set has multiple wavelengths cut and warn
  if (use.z == "spc" && nwl (data) > 1 &&
      !any (grepl (".wavelength", c(as.character (x),
                                    as.character (dots$groups),
                                    as.character (dots$subset))))) {
    
    data <- data [, , 1, wl.index = TRUE]
    warning ("Only first wavelength is used for plotting")
  }
  
  dots <- modifyList (list (xlab = data@label [[use.x]],
                            ylab = data@label [[use.y]]),
                      dots)

  if (any (grepl ("spc", c(as.character (x),
                           as.character (dots$groups),
                           as.character (dots$subset))))){
    data <- as.long.df (data, rownames = TRUE)
  } else {
    data <- data$..
    data$.rownames <- as.factor (rownames (data))
  }


  
  if (is.factor (data [[use.z]]) && transform.factor) {
    dots <- trellis.factor.key (data [[use.z]], dots)
    data [[use.z]] <- as.numeric (data [[use.z]])
  }
  
  do.call(levelplot, c (list (x, data), dots))
}

setMethod ("levelplot", signature (x = "hyperSpec", data = "missing"),
           function (x, data, ...) {
             .levelplot (x = formula (spc ~ .wavelength * .row), data = x, ...)
           })
setMethod ("levelplot", signature (x = "formula", data = "hyperSpec"), .levelplot)

