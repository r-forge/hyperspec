library(gWidgets)

plots.gui <- function(spc, spikiness, npts = 10, nspc = 1,
                      save.tmp = 20, use.tmp = FALSE, ispikes = NULL, iispikes = NULL) {
 
  ### The following is pretty much from spikefilter.R
  
  wavelength <- spc@wavelength
  spc <- spc@data$spc
  gc ()
  
  dim <- dim (spikiness) 
  dim(spikiness)  <- NULL
  
  if (is.null (ispikes))
    ispikes <- order (spikiness, na.last = TRUE, decreasing = TRUE)

  if (is.null (iispikes))
    iispikes <- order (ispikes)
  
  start.i = 1
  i <- start.i
  
  ### If use.tmp moved into button handler
  
  save.i <- 1
  if (save.tmp > 0)
    save.tmp = save.tmp + 1
  
  ### End copy-pasta
  
  options("guiToolkit"="Qt")
  
  window <- gbasicdialog("plots.gui - gWidgets (modal)", do.buttons=FALSE)
  wgroup <- ggroup(horizontal=FALSE, cont=window)
  pgroup <- gpanedgroup(container=wgroup)
  ggmain <- ggraphics(width=400, height=400, cont=pgroup)

  rgroup <- ggroup(horizontal=FALSE, cont=pgroup)
  ggsub1 <- ggraphics(width=200, height=200, cont=rgroup)
  ggsub2 <- ggraphics(width=200, height=200, cont=rgroup)
## name ggraphics to call visible later
  size(pgroup) <- c(650, 450) ## fix for scrollbars
  
  status <- gstatusbar("Suspicion 1 of 100", cont=window)
  
  selectPts <- function(h, ...) {
    
    hx <- h$x
    hy <- h$y
    
    selected <<- (x >= hx[1]) & (x <= hx[2]) &
           (y >= hy[1]) & (y <= hy[2])
    
    updatePlots()
  }
  selected <- NULL
  addHandlerChanged(ggmain, handler=selectPts)
  
  nextSuspicion <- function(...) {
    
    
  }
  
  plotMain <- function(...) {
    
    visible(ggmain) <- TRUE
    par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
    plot (wavelength, spc[ind[1],], ylim = range (spc[k,], na.rm = TRUE), type = "n")
    for (l in k)
      lines (wavelength, spc[l,], col = if (l == ind[1]) "blue" else "black")
      
    points (wavelength[ind[2]], spc[ind[1], ind[2]], col = "red", pch = 20)
  }
  plotSub1 <- function(...) {
    
    visible(ggsub1) <- TRUE
    par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
    j <- ind[2] + (-npts : npts) # suspicious data points plus points around
    j <- j[j > 0]
    j <- j[j <= ncol (spc)]

    x <- range (wavelength[j], na.rm = TRUE)
    x <- c(x[1], (x[2] - x[1]) * 1.1 + x[1])

    yl <- min (spc[k,j,drop = FALSE], na.rm = TRUE)
    print (yl)
    yu <- median (spc[k,j,drop = FALSE], na.rm = TRUE)
    yu <- (yu - yl) * 4 + yl
    print (yu)

##     plot (spc[k, , j, index = TRUE], "spc", 
##           col = c("black", "blue", "black"), pch = 20, type = "p",
##           xlim = x,
##           ylim = y,
##           cex = 0.5)
##     plot (spc[ind[1], , j, index = TRUE], "spc", 
##           col = "blue", pch = 20, type = "p",
##           add = TRUE)
   plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = c (yl, yu), type = "n")
    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20,
             type = "p",
             cex = 0.5,
             col = if (l == ind[1]) "blue" else "black")
    lines (wavelength[j], spc[ind [1],j], col = "blue")        
    points (wavelength[j], spc[ind[1], j], 
          col = "blue", pch = 20, type = "p"
          )

  }
  plotSub2 <- function(...) {
    
    visible(ggsub2) <- TRUE
    par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
    
    j <- ind[2] + (-npts : npts) # suspicious data points plus points around
    j <- j[j > 0]
    j <- j[j <= ncol (spc)]

    x <- range (wavelength[j], na.rm = TRUE)
    x <- c(x[1], (x[2] - x[1]) * 1.1 + x[1])
    
   y <- range (spc[k,j,drop = FALSE], na.rm = TRUE)
  
   plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = y, type = "n")

    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20, type = "p", cex = 0.5,
          col = if (l == ind[1]) "blue" else "black")
          
    points (wavelength[j], spc[ind[1], j], 
          col = "blue", pch = 20, type = "p"
          )

    x <- rep (x[2], 3) 
    y <- (10:8)/10 * (y[2] - y [1]) + y[1]

  }
    
  updatePlots <- function(...) {
    
    svalue(status) <- paste("Spike suspicion",i,"of",length(ispikes))
    
    if (is.na (ispikes[i])) {
      nextSuspicion()
      return
    }
    
    ind <<- vec2array (ispikes [i], dim = dim)
    
    nspc <- svalue(gnspc)
    k <<- ind[1] + (-nspc : nspc) # suspicious spectrum plus the spectra around
    k <<- k [k > 0]
    k <<- k [k <= nrow (spc)]
    isna <- apply (spc[k,,drop = FALSE], 1, function (x) all (is.na (x)))
    k <<- k[! isna]
    
    
    plotMain()
    plotSub1()
    plotSub2()
    #visible(ggmain) <- TRUE
    #par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
    #plot(x,y)
    #if(any(selected))
    #  points(x[selected],y[selected], pch=20, col="red")
    #  points(x[selected],y[selected], pch=21)
##    visible(ggsub1) <- TRUE
##    par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
##    hist(x)
##    if(any(selected))
##      points(x[selected],y[selected], pch=20, col="red")
##      points(x[selected],y[selected], pch=21)
##    visible(ggsub2) <- TRUE
##    par(mar=c(3,3,2,1), mgp=c(2,0.7,0), tck=-0.01)
##    hist(y)
##    if(any(selected))
##      points(x[selected],y[selected], pch=20, col="red")
##      points(x[selected],y[selected], pch=21)
  }
  
  updateData <- function(...) {
    
    y <<- rnorm(100)
    x <<- pnorm(y)
    selected <<- NULL
    updatePlots()
  }
  x <- NULL
  y <- NULL
  
  tmp <- ggroup(cont=wgroup)
  add(tmp, gbutton("New Data", handler=updateData))
  add(tmp, gbutton("Done", handler=function(...) {
    dispose(window)
    dev.off(); dev.off(); dev.off(); ### This isn't' working as hoped, try it to see
  }))
  
  tmp <- gframe("Number of Species", cont=wgroup)
  gnspc <- gslider(from=1,to=20,by=1,value=1, cont=tmp, handler=function(...){
    svalue(lnspc) <- paste("(",svalue(gnspc),")",sep='');
    updatePlots()
  })
  lnspc <- glabel("(1)", cont=tmp)

  tmp <- gframe("Spikes", container=wgroup)
  add(tmp, gbutton("Next Suspicion", handler=function(...){i<<-i+1; updatePlots()}))
  add(tmp,gimage('forward'))
  add(tmp, gbutton("Good Spectrum"))
  add(tmp,gimage('forward'))
  add(tmp, gbutton("Bad Spectrum"))
  add(tmp,gimage('dismiss'))
  add(tmp, gbutton("Done", handler=function(...) {
    dispose(window)
    dev.off(); dev.off(); dev.off(); ### This isn't' working as hoped, try dev.list() to see
  }))
  add(tmp,gimage('ok'))

  goutput <- glabel("<b>Spike suspicion</b>: 1<br />
                     <b>Spectrum</b>: 67<br />
                     (1560.94, 2219.1)<br />
                     <b>Spike points</b>:  479 481<br />
                     <b>Spikiness</b>:  0.06663623 -1.423397",
                     markup = TRUE, cont=wgroup)


  updateData()
  visible(window, set=TRUE)
  
  return (selected)
}

#plots.gui("RGtk2", FALSE)
#plots.gui("RGtk2", TRUE)
x<-plots.gui(cartilage[1:100], scores[1:100, ])
print(x)
#plots.gui("Qt", TRUE)