spikes.interactive.cb <- function (x, spikiness, npts = 10, nspc = 1, zoomfactor = 4){
  ## GUI global variables
  ## prepare matrices for faster work
  wl <- x@wavelength
  tspc <- t (x@data$spc)
  
  n <- 0                                # current suspicion (row of spikiness)
  ispc <- integer (0)                   # current spectrum
  ispcplot <- NA                        # spectra to plot
  cols <- NA                            # colors for the spectra

  iwlplot <- NA                         # wavelengths to plot in zoomed plots
  iwlsel <- integer (0)                 # selected points

  spikes <- matrix (nrow = 0, ncol = 2, dimnames = list (NULL, c ("n", "iwl")))
                                          

  colsel <- function (iwlplot, iwlsel) {
    cols <- rep ("blue", length (iwlplot))
    cols [iwlplot %in% iwlsel] <- "red"

    cols
  }
  
  calcspc <- function (nspc = nspc) {
    ispcplot <<- ispc + (-nspc : nspc)
    cols <<- rep ("black", nspc)
    cols <<- c (cols, "blue", cols)
    cols <<- cols [ispcplot > 0 & ispcplot <= ncol (tspc)]
    ispcplot <<- ispcplot [ispcplot > 0 & ispcplot <= ncol (tspc)]
  }

  calcwl <- function (npts = svalue (gnpts)) {
    iwlsel <<- spikiness [n, "col"]
    iwlplot <<- spikiness [n, "col"] + (-npts : npts)
    calczoom ()
  }

  calczoom  <- function () {
    f <- fivenum (tspc [iwlplot, ispcplot], na.rm = TRUE)
    gzoom [] <- seq (f [5], f [1], length.out = 100)
    svalue (gzoom, index = TRUE) <-  round ((f [3] - f [1]) * zoomfactor  / (f [5] - f [1]) * 100)
  }
  
  nextSuspicion <- function (...) {
    spikes <- rbind (spikes, cbind (rep (n, length (iwlsel)), iwlsel))
  #  iwlsel <<- integer (0)
    
    n <<- n + 1
    ispc  <<- spikiness [n, "row"] 

    calcspc (svalue (gnspc))
    calcwl (svalue (gnpts))
    calczoom ()
    
    plotMain ()
  }

  plotMain <- function (...) {
    visible (ggmain) <- TRUE 
    par (mar = c(1.5,1,0,0), mgp = c (0.25,0.25,0))#, tck =-0.01)

    matplot(wl, tspc [, ispcplot], type = "l", col = cols, lty = 1, xlab = NA, ylab = NA)
    points (wl [iwlsel], tspc [iwlsel, ispc], col = "red", pch = 8)

    plotSubs (...) 
  }

  plotSubs <- function (...) {

    ## plot subplots against index !!!
    visible (ggsubzoom) <- TRUE
    par(mar = rep (0, 4), mgp = c(0, 0, 0), tck = 0)

    lower <- gzoom [] [1]
    upper <- svalue (gzoom)
    
    tmp <- tspc [iwlplot, ispcplot]
    tmp [tmp > upper] <- upper
    matplot (iwlplot, tmp, type = "l", col = cols, lty = 1, xlab = NA, ylab = NA,
             ylim = c (lower, upper))

    tmp <- tspc [iwlplot, ispc]
    tmp [tmp > upper] <- NA
    points  (iwlplot, tspc [iwlplot, ispc    ],
             col = colsel (iwlplot, iwlsel), pch = 20, type = "p")
    
    visible (ggsub) <- TRUE
    par(mar = c(0.1,0.1,0.1,0.1), mgp = c(0, 0, 0), tck =-0.01)
    
    matplot (iwlplot, tspc [iwlplot, ispcplot], type = "l", col = cols, lty = 1, xlab = NA, ylab = NA)
    points  (iwlplot, tspc [iwlplot, ispc    ],
             col = colsel (iwlplot, iwlsel), pch = 20, type = "p")
  }  
  
  selectPts <- function (h, ...) {
    h$x [1] <- max (h$x [1], min (iwlplot))
    h$x [2] <- min (h$x [2], max (iwlplot))
    h$y [1] <- max (h$y [1], min (tspc [iwlplot, ispc]))
    #browser ()
    h$y [2] <- min (h$y [2], max (iwlplot))
    
   region <- ceiling (h$x [1]) : floor (h$x [2])

 #  mode <- svalue (selmode)
   ## for toggle better do y selection.
 #  if (mode == "union")
     region <- region [tspc [region, ispc] >= h$y [1]]
#   else
#     region <- region [(tspc [region, ispc] > h$y [1]) & (tspc [region, ispc] <= h$y [2])]
#   switch (mode,
#           union:
           iwlsel <<- union (iwlsel, region)#,
#           intersect: iwlsel <<- intersect (iwlsel, region),
#           diff: iwlsel <<- setdiff (iwlsel, region)
#           )

   plotSubs ()
  }
  
  togglePts <- function (h, ...) {
    ## TODO
   plotSubs ()
  }


  ## layout for plots
  window <- gbasicdialog ("spikefilter", buttons = "")
    
  wgroup <- ggroup (horizontal = FALSE, cont = window)

  ## plots
  pgroup <- gpanedgroup (container = wgroup)
  size (pgroup) <- c(1250, 450) ## fix for scrollbars
  maingroup <- ggroup (container = pgroup, horizontal = FALSE)
  ggmain <- ggraphics (width = 800, height = 400, cont = maingroup)

  
  rgroup <- gpanedgroup (horizontal = FALSE, cont = pgroup)
  rrgroup <- ggroup (cont = rgroup)
  ggsub <- ggraphics (width = 400, height = 200, cont = rrgroup)
#  addSpace (rrgroup, )
 
  vzoomgroup <- ggroup (cont = rgroup, horizontal = FALSE)
  hzoomgroup <- ggroup (cont = vzoomgroup)
  ggsubzoom <- ggraphics (width = 400, height = 200, cont = hzoomgroup)

  status <- gstatusbar ("Click the main plot to redraw.", cont = window)

  addhandlerclicked (ggmain, handler = nextSuspicion) # only as workaround for initial display bug

  visible (ggsubzoom) <- TRUE
  addHandlerChanged (ggsubzoom, handler = selectPts)
  #addhandlerclicked (ggsubzoom, handler = togglePts)

  visible (ggsub) <- TRUE
  addHandlerChanged (ggsub, handler = selectPts)
  #addhandlerclicked (ggsub, handler = togglePts)

  
  ##  tmp <- gframe("Display settings", cont = wgroup)
  nspcgroup <- ggroup (cont = maingroup)
  add (nspcgroup, glabel ("suspicion +/- "))
  gnspc <- gslider(from = 0, to = 20, by = 1, value = nspc, cont = nspcgroup,
                        handler = function (...){
                          calcspc (svalue (gnspc))
                          plotMain ()
                        }, expand = TRUE)
  add (nspcgroup, glabel (" spectra"))

  nptsgroup <- ggroup (cont = vzoomgroup)
  add (nptsgroup, glabel ("suspicion +/- "))
  gnpts <- gslider(from = 0,to = 20,by = 1,value = npts, cont = nptsgroup, 
                                   handler = function(...){
                                     calcwl (svalue (gnpts))
                                     plotMain ()
                                   }, expand = TRUE)
  add (nptsgroup, glabel (" points"))
       
  gzoom <- gslider (cont = hzoomgroup,
                    horizontal = FALSE,
                    handler = function(...){
                      plotSubs ()
                    }, expand = TRUE)

#  selmode <- gradio( c("union", "intersect", "diff"), selected = "union")
       
#  nextSuspicion ()  
  visible (window, handler = function (...){}) <- TRUE # runs the dialog

  spikes [, "n"] <- spikiness [spikes [, "n"], "row"]
  colnames (spikes) <- c ("ispc", "iwl")

  spikes
}
#debug (spikes.interactive.cb)
#spikes.interactive.cb (cartilage, suspicions)
