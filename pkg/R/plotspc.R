###--------------------------------------------------------------------------------------------------
###
###  plotspc - Plots spectra of hyperSpec object
###
###  convenient plot interface for plotting spectra
###

plotspc <- function  (object,
                       ## what wavelengths to plot
                      wl.range = NULL, wl.index = FALSE,  wl.reverse = FALSE,
                      ## what spectra to plot
                      spc.nmax = 50, func = NULL, func.args = list (),
                      stacked = NULL, stacked.args = list (),
                      ## plot area
                      add = FALSE, bty = "l", plot.args = list(),
                      ## lines
                      col = "black", lines.args = list (),
                      ## axes
                      xoffset = 0, yoffset = 0, nxticks = 10, axis.args = list (),
                      break.args = list (),
                      ## title (axis labels)
                      title.args = list (),
                      ## parameters for filled regions
                      fill = NULL, fill.col = NULL, border = NA, polygon.args = list (),
                      ## line indicating zero intensity
                      zeroline = list (lty = 2, col = col)){
  force (zeroline) # otherwise stacking messes up colors
  if (is.null (zeroline))
    stop ("NULL zeroline")
  
  chk.hy (object)
  validObject (object)
  if (nrow (object) == 0) stop ("No spectra.")

  ## prepare wavelengths ............................................................................
  ## somewhat more complicated here because of plotting with cut wavelength axis
  if (is.null (wl.range)) {
    wl.range <- seq_along (object@wavelength)
    wl.index <- TRUE
  }
  
  if (!is.list (wl.range))
    wl.range <- list (wl.range)

  if (!wl.index)
    wl.range <- lapply (wl.range, function (w) {
      tmp <- unique (wl2i (object, w))
      tmp [! is.na (tmp)]
    })

  ## xoffset ........................................................................................
  ## may be
  ## - one number for all wl.ranges
  ## - a number for each wl.range
  ## - one less than wl.ranges: first will be 0
  if (length (xoffset) == length (wl.range) - 1)
    xoffset = c (0, xoffset)
  else if (length (xoffset) == 1)
    xoffset = rep (xoffset, times = length (wl.range))
  if (!is.numeric(xoffset) || (length (xoffset) != length (wl.range)))
    stop ("xoffset must be a numeric  vector of the same length (or one less) as the list with",
          "wavenumber ranges.")
  xoffset <- cumsum (xoffset)

  ## for indexing wavelength.range is needed unlisted
  u.wl.range <- unlist (wl.range)

  ## wavelengths are the numbers to print at the x axis
  wavelengths <- relist (object@wavelength [u.wl.range], wl.range)

  ## x are the actual x coordinates
  x <- wavelengths
  for (i in seq_along(x))
    x [[i]] <- x [[i]] - xoffset[i]

  ## prepare spectra ................................................................................
  ## indices into columns of spectra matrix spc
  ispc <- relist (seq_along (u.wl.range), wl.range)

  rm (wl.range)
  spc <- object[[,, u.wl.range, drop = FALSE, wl.index = TRUE]]
  rm (u.wl.range)


  ## summary statistics: apply function func to spc
  if (!is.null (func)){
    if (!is.function (func))
      stop ("func needs to be a function.");

    apply.args <- c (list (X = spc, MARGIN = 2, FUN = func), func.args)
    spc <- matrix (do.call (apply, apply.args),  #apply (spc, 2, func),
                   ncol = ncol (spc)
                   )
    if (nrow (spc) == 0)
      stop ("No spectra after", func, "was applied.")
  }

  ## do not plot too many spectra by default: can take very long and there is most probably nothing
  ## visible on the resulting picture
  if (nrow (spc) > spc.nmax){
    warning (paste ("Number of spectra exceeds spc.nmax. Only the first",
                    spc.nmax, "are plotted."))
    spc <- spc [seq_len (spc.nmax), , drop = FALSE]
  }

  ## stacked plot
  if (!is.null (stacked)){
    stacked.args <- modifyList (stacked.args,
                                list (x = object, stacked = stacked, .spc = spc))
    
    if (! is.null (lines.args$type) && lines.args$type == "h")
      stacked.args <- modifyList (stacked.args, list (min.zero = TRUE))

    stacked <- do.call (stacked.offsets, stacked.args)
    if (all (yoffset == 0))
      yoffset <- stacked$offsets [stacked$groups]
    else if (length (yoffset) == length (unique (stacked$groups)))
      yoffset <- yoffset [stacked$groups]
  }

  ## yoffset ........................................................................................
  ## either one value for all spectra
  ## or one per spectrum or one per group
  if (length (yoffset) != nrow (spc)){
    if (length (yoffset) == 1)
      yoffset <- rep (yoffset, nrow (spc))
    else if (length (yoffset) > nrow (spc))
      yoffset <- yoffset [seq_len (nrow (spc))]
    else
      stop ("yoffset must be single number or one number for each spectrum (or stacking group).")
  }
  
  spc <- sweep (spc, 1, yoffset, "+")

  ## plot area --------------------------------------------------------------------------------------

  ## should a new plot be set up?
  if (! add){
    ## set default plot args
    plot.args <- modifyList (list (xlim = range (unlist (x), na.rm = TRUE),
                                   ylim = range (spc, na.rm = TRUE)),
                             plot.args)
    
    ## the actual spectra are plotted below, so we do not need any line parametrers here

    ## reverse x axis ?
    if (wl.reverse)
      plot.args$xlim <- rev(plot.args$xlim)

    ## some arguments must be overwritten if given:
    plot.args <- modifyList (plot.args,
                             list (x = unlist (x), y = spc[1,,drop=FALSE],
                                   type = "n", bty = "n",
                                   xaxt = "n", yaxt = "n", # axes and title are called separately 
                                   xlab = NA,  ylab = NA)) # for finer control
    
    do.call (plot, plot.args)

                             ## reversed x axis leads to trouble with tick positions
                             ## 
    if (diff (plot.args$xlim) < 0)
      plot.args$xlim <- rev(plot.args$xlim)

    ## Axes -----------------------------------------------------------------------------------------
    axis.args <- modifyList (list (x = list (), y = list ()), axis.args)

    ## x-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "]", "x") ){
      cuts <- .cut.ticks (sapply (wavelengths, min), sapply (wavelengths, max), xoffset, nxticks)
      
      axis.args$x <- modifyList (axis.args [! names (axis.args) %in% c ("x", "y")],
                                 axis.args$x)
      if (is.null (axis.args$x$labels) & ! is.null (axis.args$x$at))
        axis.args$x$labels <- axis.args$x$at
      axis.args$x <- modifyList (list (side = 1, at = cuts$at, labels = cuts$labels),
                                 axis.args$x)

      axis (side = 1, at = max (abs (plot.args$xlim)) * c(-1.1, 1.1))
      do.call (axis, axis.args$x)
      
      ## plot cut marks for x axis
      break.args <- modifyList (list (style = "zigzag"), break.args)
      break.args$axis <- NULL
      break.args$breakpos <- NULL

      if (length (cuts$cut) > 0) {
        if (! require (plotrix)){
          cat ("hyperSpec will use its own replacement for plotrix' axis.break\n\n")
          break.fun <- .axis.break
        } else {
          break.fun <- axis.break
        }
        for (i in cuts$cut)
          do.call (break.fun, c (list (axis = 1, breakpos = i), break.args))
      }
    }

    ## y-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "y")){
      axis.args$y <- modifyList (axis.args [! names (axis.args) %in% c ("x", "y")],
                                 axis.args$y)

      ## default for stacked plots is marking the groups
      if (!is.null (stacked)){
        if (! is.null (stacked.args$min.zero) && stacked.args$min.zero)
          group.mins <- stacked$offsets
        else
          group.mins <- apply (spc[!duplicated (stacked$groups),, drop = FALSE], 1, min, na.rm = TRUE)
        
        axis.args$y <- modifyList (list (at = group.mins,
                                         labels = stacked$levels [!duplicated (stacked$levels)]),
                                   axis.args$y)
      }
      
      axis.args$y <- modifyList (list (side = 2), axis.args$y)
      axis (side = 2, at = max (abs (plot.args$ylim)) * c(-1.1, 1.1))
      do.call (axis, axis.args$y)
    }

    ## Title: axis labels ---------------------------------------------------------------------------

    tmp <- title.args [! names (title.args) %in% c ("x","y", "ylab")]
    tmp <- modifyList (tmp, as.list (title.args$x))
    tmp <- modifyList (list (xlab = I(object@label$.wavelength), line = 2.5), tmp)
    do.call (title, tmp)
    
    tmp <- title.args [! names (title.args) %in% c ("x","y", "xlab")]
    tmp <- modifyList (tmp, as.list (title.args$y))
    tmp <- modifyList (list (ylab = I(object@label$spc)), tmp)
    do.call (title, tmp)
  }

  ## plot the spectra -------------------------------------------------------------------------------
  
  ## if necessary, recycle colors
  col <- rep (col, each = ceiling (nrow (spc) / length (col)), length.out = nrow (spc))

 
  ## should the intensity zero be marked?
  if (!is.logical (zeroline) && !is.na (zeroline)){
    zeroline <- modifyList (list (h = unique (yoffset)), as.list (zeroline))
    do.call (abline, zeroline)
  }

  ## start loop over wavelength ranges
  for (i in seq_along (x)){
    ## filling for polygons ........................................................................

    ## groupings for upper and lower bound of the bands
    if (!is.null (fill)){
      if (is.character (fill) && length (fill) == 1)
        fill <- unlist (object [[, fill]])
      else if (isTRUE (fill)){
        fill <- seq_len (nrow (spc) / 2)
        if (nrow (spc) %% 2 == 1) # odd number of spectra
          fill <- c (fill, NA, rev (fill))
        else
          fill <- c (fill, rev (fill))
      } else if (is.factor (fill))
        fill <- as.numeric (fill)
      else if (!is.numeric (fill))
        stop ("fill must be either TRUE, the name of the extra data column to use for grouping,",
              "a factor or a numeric.")

      groups = unique (fill)
      groups = groups [!is.na (groups)]


      polygon.args <- modifyList (list (x = NULL, y = NULL),
                                  polygon.args)

      ## fill color
      if (is.null (fill.col)){
        fill.col <- character (length (groups))

        for (j in seq_along (groups)){
          tmp <- which (fill == groups [j])
          fill.col [j] <- rgb( t (col2rgb (col [tmp[1]]) / 255) / 3 + 2/3)
        }
      } else {
        fill.col <- rep (fill.col, length.out = length (groups))
      }

      border <- rep (border, length.out = length (groups))

      polygon.args$x <- c (x [[i]], rev (x [[i]]))
      
      for (j in seq_along (groups)){
        tmp <- which (fill == groups [j])
        polygon.args$y <- c (spc[head(tmp, 1), ispc[[i]]], rev (spc [tail (tmp, 1), ispc[[i]]]))
        polygon.args$col = fill.col [groups [j]]
        polygon.args$border <- border [groups [j]]

        do.call (polygon, polygon.args)
      }
    }

    ## lines ........................................................................................

    lines.args <- modifyList (list (x = NULL, y = NULL, type = "l"), lines.args)

    if (lines.args$type == "h" && is.list (stacked)) {
    ## specialty: lines from the stacked zero line on!
      for (j in seq_len (nrow (spc))){
        keep <- ! is.na (spc [j, ispc[[i]]])
        lines.args$x <- rep (x[[i]] [keep], each = 3)
        lines.args$y <- as.numeric (matrix (c (rep (yoffset [j], sum (keep)),
                                               spc [j, ispc[[i]]] [keep],
                                               rep (NA, sum (keep))),
                                            byrow = TRUE, nrow = 3))
        lines.args$type = "l"
        lines.args$col <- col [j]
        do.call (lines, lines.args)
      }
    } else {
      for (j in seq_len (nrow (spc))){
        keep <- ! is.na (spc [j, ispc[[i]]])
                
        lines.args$x <- x[[i]][keep]
        lines.args$y <- spc [j, ispc[[i]]] [keep]
        lines.args$col <- col [j]
        
        do.call (lines, lines.args)
      }
    }
  }

  ## return some values that are needed by spc.identify
  invisible (list (x = rep (unlist (x), each = nrow (spc)) ,
                   y = spc,
                   wavelengths = rep (unlist (wavelengths), each = nrow (spc))
                   )
             )
}


