###-----------------------------------------------------------------------------
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

  validObject (object)

  if (nrow (object) == 0)
    stop ("No spectra.")

  ## prepare wavelengths
  if (is.null (wl.range)){
    wl.range <- list (seq (along = object@wavelength));
  } else {
    if (!wl.index){
      for (i in seq (along = wl.range))
        wl.range[[i]] <- wl2i (object, wl.range[[i]])
    }

    if (!is.list (wl.range))
      wl.range <- list (wl.range)

    for (i in seq (along = wl.range))
      if (!is.numeric (wl.range[[i]]) && !is (wl.range [[i]], "formula"))
        stop ("wavelength ranges need to be numeric or formulas.")

    for (i in seq (along = wl.range))
      wl.range[[i]] <- unique (wl.range[[i]][!is.na (wl.range[[i]])])
  }

  ## xoffset
  if (length (xoffset) == length (wl.range) - 1)
    xoffset = c (0, xoffset)
  else if (length (xoffset) == 1)
    xoffset = rep (xoffset, times = length (wl.range))
  if (!is.numeric(xoffset) || (length (xoffset) != length (wl.range)))
    stop ("xoffset must be a numeric  vector of the same length as the list with wavenumber ranges.")
  xoffset <- cumsum (xoffset)

  ## for indexing wavelength.range is needed unlisted
  u.wl.range <- unlist (wl.range)

  ## wavelengths are the numbers to print at the x axis
  wavelengths <- relist (object@wavelength [u.wl.range], wl.range)

  ## x are the actual x coordinates
  x <- wavelengths
  for (i in seq_along(wl.range))
    x [[i]] <- x [[i]] - xoffset[i]

  ## indices into columns of spectra matrix spc
  ispc <- relist (seq_len (length (u.wl.range)), wl.range)

  rm (wl.range)
  spc <- object[[,, u.wl.range, drop = FALSE, wl.index = TRUE]]
  rm (u.wl.range)


  ## apply function func to spc
  if (!is.null (func)){
    if (!is.function (func))
      stop ("func needs to be a function.");

    apply.args <- c (list (X = spc, MARGIN = 2, FUN = func), func.args)
    spc <- matrix (do.call (apply, apply.args),  #apply (spc, 2, func),
                   ncol = ncol (spc)
                   )
    if (nrow (spc) == 0)
      stop ("No spectra after func was applied.")
  }



  if (length (yoffset) != nrow (spc)){
    if (length (yoffset) == 1)
      yoffset <- rep (yoffset, nrow (spc))
    else
      stop ("yoffset must be single number.")
  }

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

  if (! add){
    ## Plot area
    plot.args$x <- unlist (x)
    plot.args$y <- spc[1,,drop=FALSE]
    plot.args$type <- "n"
    plot.args$bty <- bty
    plot.args$xaxt <- "n"
    plot.args$yaxt <- "n"
    plot.args$xlab <- NA     # title is called later
    plot.args$ylab <- NA

    if (is.null (plot.args$xlim))
      plot.args$xlim <- range (unlist (x), na.rm = TRUE)

    if (is.null (plot.args$ylim))
      plot.args$ylim <- range (spc, na.rm = TRUE)

    ## reverse x axis ?
    if (wl.reverse)
      plot.args$xlim <- rev(plot.args$xlim)

    do.call (plot, plot.args)

    ## reversed x axis ? => would lead to trouble with tick positions
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
        axis.args$y$at <- apply (spc[!duplicated (stacked$groups),], 1, min) #apply (spc, 1, min)
        axis.args$y$labels <- stacked$levels #seq_len (nrow (spc))
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

###-----------------------------------------------------------------------------
###
###  stacked.offsets
###

stacked.offsets <- function (x, stacked = TRUE, .spc = NULL){
  lvl <- NULL

  if (is.character (stacked))
    stacked <- unlist (x [[, stacked]])
  else if (isTRUE (stacked))
    stacked <- seq (nrow (x@data))

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
    offset[, i] <- range (.spc [stacked == groups[i], ], na.rm = TRUE)


  offset [2,] <- offset[2,] - offset [1,]
  offset <- c(-offset[1,], 0) + c(0, cumsum (offset[2,]))
  list (offsets = offset [seq (length (groups))],
        groups = stacked,
        levels = if (is.null (lvl)) stacked else lvl
        )
}

###-----------------------------------------------------------------------------
###
### spc.identify
###
###

spc.identify <- function (x, y = NULL, wavelengths = NULL, ispc = NULL, ...){
  if (is.list (x)) {
    if (is.null (wavelengths))
      wavelengths <- x$wavelengths
    if (is.null (y))
      y <- x$y
    x <- x$x
  }

  if ((length (x) != length (y)) | (length (x) != length (wavelengths)))
    stop ("x, y, and wavelength need to have the same length.")

  if (is.null (ispc))
    ispc <- row (y)
  else
    ispc <- ispc[row(y)]

  i <- identify (x, y,
                 labels = paste (ispc, format (wavelengths, digits = 4),
                   sep = ", "),
                 ...
                 )

  data.frame (i = ispc [i], wavelengths = wavelengths [i])
}


