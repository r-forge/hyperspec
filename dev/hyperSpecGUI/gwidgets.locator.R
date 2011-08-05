library(gWidgets)
options("guiToolkit"="Qt")

win <- gwindow()
grp <- ggroup(container=win)
size(grp) <- c(75*6, 75*6)
gfx <- ggraphics(container=grp)
bar <- gstatusbar("Ready",container=win)

drawPlot <- function(ind) {
  
  plot(y)
  if(!missing(ind) && any(ind))
    points(x[ind],y[ind], pch=20, col="red")
    points(x[ind],y[ind], pch=21)
}

addHandlerChanged(gfx, handler=function(h) {
  
  hx <- h$x
  hy <- h$y
  
  ind <- (x >= hx[1]) & (x <= hx[2]) &
         (y >= hy[1]) & (y <= hy[2])

  print(h)
  drawPlot(ind)
  #plot(y)
  #points(x[ind],y[ind],col=2)
  
  svalue(bar) <- paste("Points:",sum(ind))
})

y <- rnorm(100)
x <- 1:100

drawPlot()