## ggplot functions
## TODO: export

qplotspc <- function (x, wl.range, ...,
                      mapping = aes (x = `.wavelength`, y = `spc`, group = `.rownames`),
                      spc.nmax = 10){
  chk.hy (x)
  validObject (x)

  ## cut away everything that isn't asked for _before_ transforming to data.frame
  if (nrow (x) > spc.nmax) {
    warning ("Number of spectra exceeds spc.nmax. Only the first ", spc.nmax, " are plotted.")
    x <- x [seq_len (spc.nmax)]
  }
  
  if (!missing (wl.range))
    x <- x [,, wl.range]

  df <- as.long.df (x, rownames = TRUE)

  # different spectral ranges go into facets
  if (!missing (wl.range)){
    ranges <- integer (nwl (x))
    for (r in seq_along (wl.range)) 
      ranges [wl2i (x, wl.range [r])] <- r
    if (any (ranges == 0))
      stop ("internal error in qplotspc: 0 range. Please contact the package maintainer.")
    ranges <- as.factor (ranges)
    df$.wl.range <- rep (ranges, each = nrow (x))
  }

  p <- ggplot (df, mapping = mapping) +
    xlab (labels (x, ".wavelength")) +
    ylab (labels (x, "spc")) +
    geom_line (...)

    if (!missing (wl.range))
      p <- p + facet_grid (. ~ .wl.range,
                           labeller = function (...) "",
                           scales = "free", space = "free") +
           opts (strip.text.x = theme_text (size = 0))

  p
}

examples (qplotspc) <- function (){
  qplotspc (chondro)
  qplotspc (paracetamol, c (2800 ~ max, min ~ 1800)) + scale_x_reverse (breaks = seq (0, 3200, 400))

  qplotspc (aggregate (chondro, chondro$clusters, mean),
            mapping = aes (x = .wavelength, y = spc, colour = clusters)) +
    facet_grid (clusters ~ .)

  qplotspc (aggregate (chondro, chondro$clusters, mean_pm_sd),
            mapping = aes (x = .wavelength, y = spc, colour = clusters, group = .rownames)) +
    facet_grid (clusters ~ .)
}

qplotmap <- function (object, mapping = aes (x = `x`, y = `y`, fill = `spc`), ...,
                      func = mean, func.args = list ()){
  chk.hy (object)
  validObject (object)

  if (nwl (object) > 1 & ! is.null (func))
    object <- do.call (apply, c (list (object, 1, func), func.args))
  ## FIXME: nice label
  
  p <- ggplot (as.long.df (object), mapping = mapping) +
    coord_equal () + geom_tile () +
    xlab (labels (object)[[as.character (mapping$x)]]) + 
    ylab (labels (object)[[as.character (mapping$x)]])   
      
  p
}
examples (qplotmap) <- function (){
  qplotmap (chondro)
}


qplotc <- function (object, mapping = aes (x = `c`, y = `spc`), ...,
                     func = NULL, func.args = list ()){
  chk.hy (object)
  validObject (object)

  dots <- list (...)

  if (! is.null (func)) 
    object <- do.call (apply, c (list (object, 1, func), func.args))
  
  ## allow to plot against the row number
  object$.row <- row.seq (object)

  ## find out whether the wavelengths are needed individually,
  ## if not, use only the first wavelength and issue a warning

  if (any (grepl ("spc", as.character (mapping))) && # use intensities
      nwl (object) > 1 &&                            # has > 1 wavelength
      is.null (func) &&                              # no stats function
      ! any (grepl ("[.]wavelength", as.character (mapping)))) {
    object <- object [,, 1, wl.index = TRUE]
    warning ("Intensity at first wavelengh only is used.")
  }

  ## produce fancy y label
  ylab <- labels (object, as.character (mapping$y))
  if (! is.null (func)) 
    ylab <- make.fn.expr (substitute (func), c (ylab, func.args))
  
  ## expand the data.frame
  df <- as.long.df (object, rownames = TRUE, wl.factor = TRUE)

  ## if plots should be grouped, faceted, etc. by wavelength, it is better to have a factor
  if (any (grepl ("[.]wavelength", mapping [! names (mapping) %in% c("x", "y")])))
    df$.wavelength <- as.factor (df$.wavelength)

  p <- ggplot (df, mapping = mapping) + geom_point () +
    ylab (ylab) +
      xlab (labels (object, as.character (mapping$x)))

  p
}

make.fn.expr <- function (fn, l = list ()){

  if (length (fn) > 1L)
    fn <- "f"

  l <- lapply (l, function (x) if (is.logical (x)) as.character (x) else x)

  if (is.null (names (l)))
    names (l) <- rep ("", length (l))
  
  tmp <- mapply (function (x, y) if (nzchar (x) > 0L) bquote (.(x) == .(y)) else y,
                 names (l), l)
  
  e <- expression (f (x))
  e [[1]][[1]] <- fn
  if (length (tmp) > 0L)
    e [[1]][seq_along (tmp) + 1] <- tmp
  else
    e [[1]][2] <- NULL

  e
}
