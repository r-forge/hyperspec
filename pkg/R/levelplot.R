#################################################################################
###
###  levelplot.R - everything that has to do with levelplot-like plotting:
###                * levelplot
###                * plotmap
###                * plotvoronoi
###                * map.identify
###  Time-stamp: <Claudia Beleites on Wednesday, 2010-01-13 at 17:11:41 on cb>
###  
###  levelplot is used by plotmap, plotmat, plotvoronoi
###  
###  Version 1.0  2009-12-16 15:04  Claudia Beleites  Claudia.Beleites@gmx.de
###
#################################################################################

### the workhorse function
.levelplot <- function (x, data, transform.factor = TRUE, ...) {
  validObject (data)

  data$.row <- seq_len (nrow (data))

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

#################################################################################
###
###  plotmap - plot spectral maps
###  
###  plots intensity or extra data column over 2 extra data columns

plotmap <- function (object, model = spc ~ x * y,
                     func = mean, func.args = list (), ...){
  .is.hy (object)
  validObject (object)

  if (nwl (object) > 1)
    object <- do.call (apply, c (list (object, 1, func), func.args))
  
  dots <- modifyList (list (aspect = "iso"),
                      list (...))
                       
  dots <- c (list (x = model, data = object), dots)

  do.call(.levelplot, dots)
}

#################################################################################
###
###  plotvoronoi - plot spectral maps with irregular point pattern
###  
###  plots intensity or extra data column over 2 extra data columns

plotvoronoi <- function (object, model = spc ~ x * y,
                         use.tripack = TRUE, mix = TRUE, ...){
  if (use.tripack && mix)
      object@data <- object@data [sample (nrow (object)),]

  dots <- modifyList (list (object = object,
                            model = model,
                            panel = "panel.voronoi",
                            prepanel = "prepanel.default.levelplot",
                            pch = 19, cex = .25,
                            use.tripack = use.tripack),
                      list (...))
  do.call (plotmap, dots)
}

#################################################################################
###
###  map.identify - identify spectra in map plot
###  

map.identify <- function (object, model = spc ~ x * y, voronoi = FALSE, ...){
  .is.hy (object)
  validObject (object)

  dots <- modifyList (list (object = object, model = model, ...),
                      list (subscripts = TRUE))
  if (voronoi) {
    dots <- modifyList (list (col = "black", border = "#00000080"),
                        dots)
    dots <- modifyList (dots, list (mix = FALSE))
    mix <- sample (nrow (object))
    dots$object <- object [mix]
    lattice <- do.call (plotvoronoi, dots)
    mix <- order (mix)
  }
  else {
    lattice <- do.call (plotmap, dots)
    mix <- seq_len (nrow (object))
  }

  print (lattice)
  trellis.focus ()

  ## PROBLEM: panel.identify does _not_ keep the order of the selection.
  ## End of input is right click, i.e. empty return value of panel.identify.
  ## "no observations within 18 points" or the like should not break the loop, though.
  ## Thus, count the warnings, and if the number increases, do not break.

  .nwarn <- function () 
    if (exists ("last.warning")) length (last.warning) else 0
  
  nwarn <- .nwarn ()
  res <- numeric (0)
  repeat {
    tmp <- panel.identify (x = lattice$panel.args.common$x [mix], 
                           y = lattice$panel.args.common$y [mix],
                           n = 1)

    if (length (tmp) == 0 && .nwarn () == nwarn)
      break
    else
      res <- c (res, tmp)

    nwarn <- .nwarn ()
  }

  res
}

