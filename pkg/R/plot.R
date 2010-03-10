###-----------------------------------------------------------------------------
###
###  plot methods
###

###-----------------------------------------------------------------------------
###
### .plot: main switchyard for plotting functions
###

.plot <-  function (x, y, ...){
  ##    'spc'        ... spectra
  ##    'map'        ... map
  ##    'voronoi'    ... voronoi tiled map
  ##    'mat'        ... spectra matrix
  ##    'c'          ... concentration: plotc
  ##    'ts'         ... time series: plotc
  ##    'depth'      ... concentration or time series
  ##    'spcmeansd'  ... mean spectrum +- 1 standard deviation
  ##    'spcprctile' ... median spectrum , 16th and 84th percentile
  ##    'spcprctl5'  ... spcprctile plus 5th and 95th percentile
  
  dots <- list(...)          # to allow optional argument checks
  
  if (missing (y)){
    stop ("second argument to plot is missing. Should be a character indicating the type of plot.")
    y = "spc"
  }
  
  switch (tolower (y),
          spc = plotspc (x, ...),
          spcmeansd = {
            dots <- modifyList (list (object = x,
                                      fill = c (1, NA, 1),
                                      func = mean_pm_sd,
                                      func.args = list (na.rm = TRUE)
                                      ),
                                dots)
            do.call (plotspc, dots)
          },
          spcprctile = {
            dots <- modifyList (list (object = x,
                                      fill = c (1, NA, 1),
                                      func = quantile,
                                      func.args = list (na.rm = TRUE,
                                        probs = c (0.16, 0.5, 0.84))
                                      ),
                                dots)
            do.call (plotspc, dots)
          },
          spcprctl5 = {
            dots <- modifyList (list (object = x,
                                      fill = c (1, 2, 3, 2, 1),
                                      fill.col = c("#00000040"),
                                      func = quantile,
                                      func.args = list (na.rm = TRUE,
                                        probs = c (0.05, 0.16, 0.5, 0.84, 0.95))
                                      ),
                                dots)
            do.call (plotspc, dots)
          },
          map = plotmap (x, ...),
          voronoi = plotvoronoi (x, ...),
          mat = .levelplot (spc ~ .wavelength * .row, x, ...),
          c = plotc (x, ...),
          ts = plotc (x, spc ~ t, ...),
          depth = plotc (x, spc ~ z, ...),
          stop (paste ("y = ", y, "unknown.", collapse = " "))
          )
}

### use plotspc as default plot function
setMethod ("plot",
           signature (x = "hyperSpec", y = "missing"),
           function (x, y, ...) plotspc (x, ...)
           )

### allow choice of spectral or map plot by second argument
setMethod ("plot",
           signature (x = "hyperSpec", y = "character"), .plot)

