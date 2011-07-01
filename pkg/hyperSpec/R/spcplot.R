## new version of plotspc

plotspc2 <- function (object,
                     ## what wavelengths to plot
               
                     spc.nmax = 10,
                     ## pre-processing of the spectra
                     
                     ## plot area
                     add = FALSE,
            
                     ...
                     ){
  
  args <- modifyList (list (wl.range = NULL, wl.index = FALSE, wl.reverse = FALSE, wl.offset = 0,
                            plot.bty = "l",
                            zeroline.lty = 2, zeroline.col = col
                            ),
                      list (...))

  args <- split.dots (args, functions = c (
                              wl = "wl",
                              stacked = "stacked",
                              plot = "plot",
                              lines = "lines",
                              title = "title",
                              breaks = "breaks",
                              polygon = "polygon",
                              zeroline = "zeroline"
                              ))

  ## prepare wl-axis
  wl <- .prep.wl (object, wl.range, wl.index, wl.offset)

  ## summary statistics:
  ## TODO: aggregate or sample spectra
  object <- object [seq_len (min (spc.nmax, nrow (spc))),, wl$i, wl.index = TRUE]
  wl$i <- NULL # not useful any longer, use indexing by wl$group instead

  ## prepare y: stacking
  ## 
 # y <- .prep.int (object, )
  
  ## prepare plotting area if necessary
  if (! add) .setup.plot (object, wl.reverse, wl$x)

  ## plot each wl-range
  
}

.prep.wl <- function (object, wl.range, wl.index, wl.offset){

  ## make sure wl.range is a list
  if (is.null (wl.range)) {
    wl.range <- seq_along (object@wavelength)
    wl.index <- TRUE
  }
  
  if (!is.list (wl.range))
    wl.range <- list (wl.range)
  
  ## make vector of wl.offset values for each wl.range
  if (length (wl.offset) == 1)
    wl.offset = rep (wl.offset, times = length (wl.range))
  else if (length (wl.offset) == length (wl.range) - 1)
    wl.offset = c (0, wl.offset)
  else if (!is.numeric(wl.offset) || (length (wl.offset) != length (wl.range)))
    stop ("wl.offset must be a numeric  vector of the same length (or one less) as the list with",
          "wavenumber ranges.")

  wl.offset <- cumsum (wl.offset)

  ## make sure we have indices in wl.range
  if (! wl.index)
    wl.range <- lapply (wl.range, function (w) wl2i (object, w))

  wl.offset <- wl.offset [! sapply (wl.range, is.null)]
  wl.range  <- wl.range  [! sapply (wl.range, is.null)]
  
  wl.group  <- mapply (function (g, n) rep (g, each = n),
                       seq_along (wl.range),
                       sapply (wl.range, length))

  data.frame (index = unlist (wl.range),
              x     = unlist (mapply (function (i, offset) object@wavelength [i] - offset,
                                      wl.range,
                                      wl.offset)),
              group = as.factor (unlist (wl.group)))
}

.prep.int <- function (spc#,
                    ){
 ## eventually, we warnt
  ## 1.a use functions that work on matrix
  ## 2. use aggregate 
  ## => package arrayhelpers or dfm
  browser ()
  if (apply)
    .apply (spc, 2, stats, ...)
  else
    do.call (stats, c (spc, list (...)))
}
