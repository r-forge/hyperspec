library(gWidgets)

plots.gui <- function(toolkit, resize) {
 
  options("guiToolkit"=toolkit)
  
  #window <- gwindow("plots.gui - gWidgets", visible=FALSE)
  window <- gbasicdialog("plots.gui - gWidgets (modal)", do.buttons=FALSE)
  wgroup <- ggroup(horizontal=FALSE, cont=window)
  pgroup <- gpanedgroup(container=wgroup)
  ggmain <- ggraphics(width=400, height=600, cont=pgroup)

  rgroup <- ggroup(horizontal=FALSE, cont=pgroup)
  ggsub1 <- ggraphics(width=300, height=300, cont=rgroup)
  ggsub2 <- ggraphics(width=300, height=300, cont=rgroup)
## name ggraphics to call visible below
#  add(pgroup, ggraphics(width=width, height=height)); ggmain <- dev.cur()
 
#  add(rgroup, ggraphics(width=width)); ggsub1 <- dev.cur()
#  add(rgroup, ggraphics(height=height)); ggsub2 <- dev.cur()
  
#  if(resize) size(pgroup) <- c(75*9, 75*6)
  
  selectPts <- function(h, ...) {
    
    hx <- h$x
    hy <- h$y
    
    ind <<- (x >= hx[1]) & (x <= hx[2]) &
           (y >= hy[1]) & (y <= hy[2])
  
    #print(h)
    #drawPlot(ind)
    #plot(y)
    #points(x[ind],y[ind],col=2)
    
    #svalue(bar) <- paste("Points:",sum(ind))
    updatePlots()
  }
  ind <- NULL
  addHandlerChanged(ggmain, handler=selectPts)
  
  updatePlots <- function(...) {
    
    visible(ggmain) <- TRUE; plot(x,y)
    #plot(y)
    if(any(ind))
      points(x[ind],y[ind], pch=20, col="red")
      points(x[ind],y[ind], pch=21)
    visible(ggsub1) <- TRUE; hist(x)
    if(any(ind))
      points(x[ind],y[ind], pch=20, col="red")
      points(x[ind],y[ind], pch=21)
    visible(ggsub2) <- TRUE; hist(y)
    if(any(ind))
      points(x[ind],y[ind], pch=20, col="red")
      points(x[ind],y[ind], pch=21)
#    dev.set(ggmain); plot(x,y)
#    dev.set(ggsub1); hist(x)
#    dev.set(ggsub2); hist(y)
  }
  
  updateData <- function(...) {
    
    y <<- rnorm(100)
    x <<- pnorm(y)
    ind <<- NULL
    updatePlots()
  }
  x <- NULL
  y <- NULL
  updateData()
  
  tmp <- ggroup(cont=wgroup)
  add(tmp, gbutton("New Data", handler=updateData))
  add(tmp, gbutton("Done", handler=function(...) dispose(window)))

  updateData()
  x<-visible(window, set=TRUE)
  
  return (ind)
}

#plots.gui("RGtk2", FALSE)
#plots.gui("RGtk2", TRUE)
x<-plots.gui("Qt", FALSE)
print(x)
#plots.gui("Qt", TRUE)