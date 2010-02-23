-----------------------------------------------------------------------------
###
###  plotspc - Plots spectra of hyperSpec object
###
###  convenient plot interface for plotting spectra
###

plotspc <- function  (object,
                      ## what wavelengths to plot
                      wl.range = NULL,
                      wl.index = FALSE,
                      wl.reverse = FALSE,
                      ## what spectra to plot
                      spc.nmax = 50,
                      func = NULL,
                      func.args = list (),
                      stacked = NULL,
                      ## plot / lines
                      add = FALSE,
                      bty = "l",
                      col = "black",
                      plot.args = list() ,
                      ## axes
                      xoffset = 0,
                      yoffset = 0,
                      nxticks = 10,
                      axis.args = list () ,
                      ## parameters for filled regions
                      fill = NULL,
                      fill.col = NULL,
                      border = NA,
                      title.args = list (),
                      polygon.args = list (),
                      lines.args = list (),
                      zeroline =  list (lty = 2, col = col)
                      ){
  force (zeroline) # otherwise stacking messes up colors

  .is.hy (object)
  validObject (object)

  if (nrow (object) == 0)
    stop ("No spectra.")

  ## prepare wavelengths
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

  ## xoffset
  ## may be
  ## - one number for all wl.ranges
  ## - a number for each wl.range
  ## - one less than wl.ranges: first will be 0
  if (length (xoffset) == length (wl.range) - 1)
    xoffset = c (0, xoffset)
  else if (length (xoffset) == 1)
    xoffset = rep (xoffset, times = length (wl.range))
  if (!is.numeric(xoffset) || (length (xoffset) != length (wl.range)))
    stop ("xoffset must be a numeric  vector of the same length (or one less) as the list with wavenumber ranges.")
  xoffset <- cumsum (xoffset)

  ## for indexing wavelength.range is needed unlisted
  u.wl.range <- unlist (wl.range)

  ## wavelengths are the numbers to print at the x axis
  wavelengths <- relist (object@wavelength [u.wl.range], wl.range)

  ## x are the actual x coordinates
  x <- wavelengths
  for (i in seq_along(x))
    x [[i]] <- x [[i]] - xoffset[i]

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

  ## yoffset
  ## either one value for all spectra
  ## or one per spectrum
  if (length (yoffset) != nrow (spc)){
    if (length (yoffset) == 1)
      yoffset <- rep (yoffset, nrow (spc))
    else
      stop ("yoffset must be single number or one number for each spectrum.")
  }

  ## do not plot too many spectra by default: can take very long and there is most probably nothing
  ## visible on the resulting picture
  if (nrow (spc) > spc.nmax){
    warning (paste ("Number of spectra exceeds spc.nmax. Only the first",
                    spc.nmax, "are plotted."))
    spc <- spc [seq_len (spc.nmax), , drop = FALSE]
    yoffset <- yoffset [seq_len (spc.nmax)]
  }

  ## stacked plot
  if (!is.null (stacked)){
    stacked <- stacked.offsets (object, stacked, spc)
    yoffset <- stacked$offsets [stacked$groups]
  }

  spc <- sweep (spc, 1, yoffset, "+")

  ## plotting

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
                                   type = "n", bty = bty,
                                   xaxt = "n", yaxt = "n", # axes and title are called separately 
                                   xlab = NA,  ylab = NA)) # for finer control
    
    do.call (plot, plot.args)

                             ## reversed x axis leads to trouble with tick positions
                             ## 
    if (diff (plot.args$xlim) < 0)
      plot.args$xlim <- rev(plot.args$xlim)

    ## Axes
    ## x-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "]") ){
      if (is.null (axis.args$x))
        axis.args$x <- list ()
      if (is.null (axis.args$x$side))
        axis.args$x$side <- 1

      ## Tick mark positions
      if (is.null (axis.args$x$at)){
        if (all (xoffset == 0)){
          axis.args$x$at <- pretty (plot.args$xlim +
                                    diff(plot.args$xlim) * c(-0.04, 0.04),
                                    nxticks)
        } else {
          axis.args$x$at <- list ()

          part <- apply (sapply (wavelengths, range), 2, diff) /
            diff (plot.args$xlim)

          for (i in seq_along (x))
            axis.args$x$at [[i]] <- pretty (wavelengths[[i]],
                                            part [i] * nxticks + 1)
        }
      }
      if (!is.list (axis.args$x$at))
        axis.args$x$at <- rep (list (axis.args$x$at), length (x))

      ## calculate cut mark positions and which ticks are to be displayed
      cutmarks <- numeric (length (x) - 1)

      for (i in seq_along (axis.args$x$at)[-1]){
        a <- max (x [[i - 1]])
        b <- min (x [[i    ]])
        delta <- b - a
        cutmarks [i - 1] <- a + delta / 2

        a <- a + xoffset [i] + delta / 4
        b <- b + xoffset [i] - delta / 4

        axis.args$x$at [[i - 1]] <- axis.args$x$at [[i - 1]][axis.args$x$at [[i - 1]] < a]
        axis.args$x$at [[i    ]] <- axis.args$x$at [[i    ]][axis.args$x$at [[i    ]] > b]
      }

      ## Tick mark labels
      if (is.null (axis.args$x$labels)){
        axis.args$x$labels <- (axis.args$x$at)
        for (i in seq_along (axis.args$x$at))
          axis.args$x$at [[i]] <- axis.args$x$at [[i]] - xoffset [i]
      }

      axis.args$x$at <- unlist (axis.args$x$at)
      axis.args$x$labels <- unlist (axis.args$x$labels)

      do.call (axis, axis.args$x)

      ## plot cut marks for x axis
      for (i in seq_along (cutmarks))
        if (xoffset[i + 1] != 0)
          mtext("//", at = cutmarks [i], side = 1, padj = -1, adj = 0.5)
    }

    ## y-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u")){
      if (is.null (axis.args$y))
        axis.args$y <- list ()
      if (is.null (axis.args$y$side))
        axis.args$y$side <- 2
      if (is.null (axis.args$y$at) & !is.null (stacked)){
        axis.args$y$at <- apply (spc[!duplicated (stacked$groups),, drop = FALSE], 1, min) 
        axis.args$y$labels <- stacked$levels 
      }

      do.call (axis, axis.args$y)
    }

    ## Title
    if (is.null (title.args$xlab))
      title.args$xlab <- list ()
    if (!is.list (title.args$xlab))
      title.args$xlab <- list (xlab = title.args$xlab)
    else
      title.args$xlab$xlab <- I(object@label$.wavelength)
    if (names (title.args$xlab) [1] == "")
      names (title.args$xlab) [1] <- "xlab"

    if (is.null (title.args$xlab$line))
      title.args$xlab$line <- 2.5

    if (is.null (title.args$ylab))
      title.args$ylab <- list ()
    if (!is.list (title.args$ylab))
      title.args$ylab <- list (ylab = title.args$ylab)
    else
      title.args$ylab$ylab <- I(object@label$spc)
    if (names (title.args$ylab) [1] == "")
      names (title.args$ylab) [1] <- "ylab"

    titles <- pmatch (c("main", "sub", "xlab", "ylab"), names (title.args))
    titles <- titles [!is.na (titles)]
    other <- !(seq (along = title.args) %in% titles)

    for (i in titles)
      do.call (title, c(title.args[[i]], title.args[other]))
  }

  col <- rep (col, each = ceiling (nrow (spc) / length (col)), length.out = nrow (spc))

  ## start loop over wavelength ranges
  for (i in seq_along (x)){
    if (!is.null (fill)){
      if (is.character (fill))
        fill <- unlist (object [[, fill]])
      else if (isTRUE (fill)){
        fill <- seq_len (nrow (spc)) / 2
        if (nrow (spc) %% 2 == 1) # odd number of spectra
          fill <- c (fill, NA, rev (fill))
        else
          fill <- c (fill, rev (fill))
      }
      if (is.factor (fill))
        fill <- as.numeric (fill)
      else if (!is.numeric (fill))
        stop ("fill must be either TRUE, the name of the extra data column to use for grouping, a factor or a numeric.")

      groups = unique (fill)
      groups = groups [!is.na (groups)]

      if (is.null(polygon.args$x))
        polygon.args <- c(list(x = NULL, y = NULL), polygon.args)

      if (is.null (fill.col)){
        fill.col <- character (length (groups))
        for (j in seq_along (groups)){
          tmp <- which (fill == groups [j])
          fill.col [j] <- rgb(t(col2rgb(col[tmp[1]]) / 255) / 3 + 2/3)
        }
      } else {
        fill.col <- rep (fill.col, length.out = length (groups))
      }

      border <- rep (border, length.out = length (groups))

      polygon.args$x <- c (x  [[i]]                  , rev (x   [[i]]         ))
      for (j in seq_along (groups)){
        tmp <- which (fill == groups [j])
        polygon.args$y <- c (spc[head(tmp, 1), ispc[[i]]], rev (spc [tail (tmp, 1), ispc[[i]]]))
        polygon.args$col = fill.col [groups [j]]
        polygon.args$border <- border [groups [j]]

        do.call (polygon, polygon.args)
      }
    }

    if (is.null(lines.args$x))
      lines.args <- c(list (x = NULL, y = NULL), lines.args)

    if (is.null (lines.args$type))
      lines.args$type <- "l"

    for (j in seq_len (nrow (spc))){
      lines.args$x <- x[[i]]
      lines.args$y <- spc [j, ispc[[i]]]
      lines.args$col <- col [j]

      do.call (lines, lines.args)
    }
  }


  if (! is.null (zeroline)){
    zeroline <- c (list (h = unique (yoffset)), zeroline)

    do.call (abline, zeroline)
  }

  invisible (list (x = rep (unlist (x), each = nrow (spc)) ,
                   y = spc,
                   wavelengths = rep (unlist (wavelengths), each = nrow (spc))
                   ))
}


