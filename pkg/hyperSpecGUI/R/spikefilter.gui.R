##' Interactive spike filtering
##'
##' These functions calculate the suspiciousness of data points by a
##' \code{c (-1, 2, -1)} filter. \code{spikefilter} applies this filter along
##' the spectrtal (wavelength) direction, \code{spikefilter2d} in addition also
##' to neighbour spectra.
##' 
##' The recognition of spikes may be greatly improved by preprocesing the
##' spectra specially for this task, see the demo.
##' @param spc hyperSpec object holding the spectra.
##' @param spikiness matrix 
##' @param npts initial wavelength axis zoom: the suspicious point +/- \code{npts}
##'        points are displayed
##' @param nspc initial number of neighbour spectra: the suspicious spectrum +/- 
##'        \code{nspc} spectra are displayed
##' @param save.tmp ignored at the moment
##' @param use.tmp  ignored at the moment
##' @param ispikes ignored at the moment
##' @param iispikes ignored at the moment
##' @rdname spikes.interactive
##' @return indices of the marked spectra
##' @export
spikes.interactive <- function (spc, spikiness, npts = 10, nspc = 1,
                                save.tmp = 20, use.tmp = FALSE, ispikes = NULL,
                                iispikes = NULL) {
  
  ## GUI global objects

  
  wavelength <- spc@wavelength
  spc <- spc@data$spc
  gc ()
  
  dim <- dim (spikiness) 
  dim (spikiness) <- NULL
  
  if (is.null (ispikes))
    ispikes <- order (spikiness, na.last = TRUE, decreasing = TRUE)

  if (is.null (iispikes))
    iispikes <- order (ispikes)

  ##  
  start.i = 1
  cur.i <- start.i
  
  save.i <- 1
  if (save.tmp > 0)
    save.tmp = save.tmp + 1
  
  
  x <- NULL  # what are these good for?
  y <- NULL  # --- not sure if need x,y any more
  j <- NULL  ## currently visible points
  selected <- rep (FALSE, times = length(wavelength))
  pts <- numeric (0)
  
  ## define the global variables for plotting
  ind <- NULL
  k <- NULL
  
  
  ## layout for plots
  window <- gbasicdialog ("spikefilter", buttons = "Done")
  wgroup <- ggroup (horizontal = FALSE, cont = window)
  pgroup <- gpanedgroup (container = wgroup)
  ggmain <- ggraphics (width = 400, height = 400, cont = pgroup)
  rgroup <- gpanedgroup (horizontal = FALSE, cont = pgroup)
  ggsub2 <- ggraphics (width = 200, height = 200, cont = rgroup)
  ggsub1 <- ggraphics (width = 200, height = 200, cont = rgroup)
  ## name ggraphics to call visible later
  size (pgroup) <- c(650, 450) ## fix for scrollbars
  
  status <- gstatusbar ("Click the main plot to redraw.", cont = window)
  
  selectPts <- function (h, ...) {
    
    w <- wavelength
    ## min/max select region
    mn <- w[min(j)]
    mx <- w[max(j)]
    ## locate points within window and selection
    ls <- (w >= h$x[1]) & (w <= h$x[2]) & (w >= mn) & (w <= mx) &
          (spc [ind [1],] >= h$y[1])   
    
    ### toggle selected points (XOR)
    selected <<- xor (selected, ls)
    
    updatePlots ()
  }
  #addHandlerChanged (ggmain, handler = selectPts) ### no need for this selection
  addhandlerclicked (ggmain, handler = updatePlots) # CB: why this?
  addHandlerChanged (ggsub1, handler = selectPts)
  addHandlerChanged (ggsub2, handler = selectPts)
  
  
  ## main plotting functions, could extract these later
  ## plotGG <- function (ggdevice, plotfn) {
  ##    ### example wrapper
  ##   visible (ggdevice) <- TRUE
  ##   plotfn()
  ## }
  plotMain <- function (...) {
    
    visible (ggmain) <- TRUE ### keep this, then call external plot command
    par (mar = c(2,2,2,1), mgp = c(2,0.7,0), tck =-0.01)
    
    plot (wavelength, spc [ind[1],], ylim = range (spc[k,], na.rm = TRUE), type = "n",
          xlab = NA, ylab = NA)
    
    for (l in k)
      lines (wavelength, spc[l,], col = if (l == ind[1]) "blue" else "black")
    points (wavelength[ind[2]], spc[ind[1], ind[2]], col = "red", pch = 20)
    
  }
  plotSub1 <- function (...) {
    
    visible(ggsub1) <- TRUE

    ## prepare data
    yl <- min (spc[k,j,drop = FALSE], na.rm = TRUE)
    
    yu <- median (spc[k,j,drop = FALSE], na.rm = TRUE)
    yu <- (yu - yl) * 4 + yl
    
    ## plot view
    par(mar = c(0.1,0.1,0.1,0.1), mgp = c(2,0.7,0), tck =-0.01)
    plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = c (yl, yu), type = "n",
          xlab = NA, ylab = NA)
    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20,
             type = "p",
             cex = 0.5,
             col = if (l == ind[1]) "blue" else "black")
    lines (wavelength[j], spc[ind [1],j], col = "blue")        
    points (wavelength[j], spc[ind[1], j], 
          col = "blue", pch = 20, type = "p")
    
    ## highlight selected points
    points (wavelength[selected], spc[ind[1], selected], 
          col = "red", pch = 20, type = "p")

  }
  plotSub2 <- function(...) {
    
    visible(ggsub2) <- TRUE
    
    ## prepare data
    y <- range (spc[k,j,drop = FALSE], na.rm = TRUE)
  
    ## plot view
    par(mar = c (0.1,0.1,0.1,0.1), mgp = c(2,0.7,0), tck =-0.01)
    plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = y, type = "n", xaxt ='n')

    ## TODO use matlines
    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20, type = "p", cex = 0.5,
             col = if (l == ind[1]) "blue" else "black")
      
    points (wavelength[j], spc[ind[1], j], 
            col = "blue", pch = 20, type = "p")
            
    ## highlight selected points
    points (wavelength[selected], spc[ind[1], selected], 
            col = "red", pch = 20, type = "p")

  }
    
  updatePlots <- function(...) {
#    browser ()

    ## update whole GUI (status bar)
    svalue(status) <- paste("Spike suspicion",cur.i,"of",length(ispikes))
    
    ## verify data
    if (is.na (ispikes[cur.i])) {
      nextSuspicion()
      return
    }
    
    ## prepare shared data
    ind <<- vec2array (ispikes [cur.i], dim = dim)
    
    #nspc <- svalue(gnspc) ### shared var is updated on change instead, which is better?
    k <<- ind[1] + (- nspc : nspc) # suspicious spectrum plus the spectra around
    k <<- k [k > 0]
    k <<- k [k <= nrow (spc)]
    isna <- apply (spc[k,,drop = FALSE], 1, function (x) all (is.na (x)))
    k <<- k[! isna]
    
    j <<- ind[2] + (-npts : npts) # suspicious data points plus points around
    j <<- j[j > 0]
    j <<- j[j <= ncol (spc)]

    x <<- range (wavelength[j], na.rm = TRUE)
    x <<- c(x[1], (x[2] - x[1]) * 1.1 + x[1])
    
    
    # redraw the 3 views
    plotMain()
    plotSub1()
    plotSub2()
  }
  nextSuspicion <- function (...) {
    
    cur.i <<- cur.i + 1
    updatePlots ()
  }
  
  
  tmp <- gframe("Surrounding spectra to display", cont = wgroup)
  gnspc <- gslider(from = 0,to = 20,by = 1,value = nspc, cont = tmp, handler = function(...){
    nspc <<- svalue(gnspc)
    updatePlots()
  }, expand = TRUE)
  tmp <- gframe("Surrounding points to display", cont = wgroup)
  gnpts <- gslider(from = 0,to = 20,by = 1,value = npts, cont = tmp, handler = function(...){
    npts <<- svalue(gnpts)
    updatePlots()
  }, expand = TRUE)

  ## GUI buttons and functions for spikes.
  tmp <- gframe("Spikes", container = wgroup)
  add(tmp, gbutton("Good Spectrum", handler = function(...){
    cur.i <<- cur.i + 1
    updatePlots()
  }))
  add (tmp, gbutton("Bad Spectrum", handler = function(...){
    cur.i <<- cur.i + 1
    spc[ind[1], ] <<- NA
    updatePlots()
  }))
  add (tmp, gbutton("Next Suspicion", handler = function(...){
    cur.i <<- cur.i + 1
    j <<- array2vec (matrix (c(rep (ind[1], dim [2]),
                               1 : dim[2]),
                             ncol = 2),
                     dim)
    ispikes [iispikes[j]] <<- NA
    
    
    ## I'm a little lost here..
    pts <- j [pts]
    spc[ind[1], pts] <- NA
    pts <- array2vec (matrix (c (rep (ind[1], length (pts)), pts), ncol = 2), 
                      dim = dim)
    ispikes [iispikes[pts]] <- NA     # do not look at this spike again
    ## End lost bit..
    
    updatePlots()
  }))
  add (tmp, gbutton("Done", handler = function(...) {
    dispose(window)
    dev.off(); dev.off(); dev.off(); ### This isn't working as hoped, try dev.list() to see
  }))
  
  ## some more buttons and their functions
  tmp <- gframe("Processing", container = wgroup)
  add(tmp, gbutton("Load save.tmp", handler = function(...){
    
    if (file.exists ("spikefilter.tmp.RData")){
      cat ("load temporary data\n")
      load ("spikefilter.tmp.RData")
    }
  }))
  add(tmp, gbutton("Save save.tmp", handler = NULL))
  add(tmp, gseparator(horizontal = FALSE)) ### can you see this?
  add(tmp, gbutton("Copy to clipboard", handler = function(...){
    names(selected) <- wavelength
    dput.to.clipboard(which(selected))
  }))

  ### the following only works with Qt
  #goutput <- glabel("<b>Spike suspicion</b>: 1<br />
  #                   <b>Spectrum</b>: 67<br />
  #                   (1560.94, 2219.1)<br />
  #                   <b>Spike points</b>:  479 481<br />
  #                   <b>Spikiness</b>:  0.06663623 -1.423397",
  #                   markup = TRUE, cont = wgroup)

  updatePlots()
  visible(window, set = TRUE)
  
  
  names(selected) <- wavelength
  return (which(selected))
}

##spikes.interactive (cartilage, scores)
