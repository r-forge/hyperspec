library(gWidgets)
library(hyperSpec)
library(cranvas)
library(plumbr) ## as.mutaframe
library(arrayhelpers)
options("guiToolkit"="RGtk2")

####################
vec2array <- function (ivec, dim) {
  ndim <- length (dim)
  pdim <- c(1, cumprod (dim))

  iarr <- matrix(NA, nrow = length(ivec), ncol = ndim) # matrix for the array indices
  colnames (iarr) <- letters[8 + seq_len (ndim)]       # i, j, k, ...

  ivec <- (ivec - 1)
  for (j in seq_len (ndim))
    iarr [, j] <- (ivec %% pdim [j + 1]) / pdim [j]

  1 + floor(iarr)
}
##################
#import(methods)
#import(stats)
#importFrom (graphics, plot)
#importFrom (lattice, levelplot)
#importFrom (grDevices, rainbow)
#export(vec2array)
library(methods)
library(stats)
############

#load("~/tmp/hyperspec/dev/spikes.workspace")

## Real notes:
##   Haven't worked out how to extend a mutaframe (row-wise)
##   ([[3]]<-c(1:5) adds col if ncol=2, nrow=5)
##   So dim(return mutaframe) must be equal to highest possible spike count

spikes.gui <- function() {
 
    ## gWidgets for Data
    sampleSize <- gradio(c(1,5,10,20,50))
    reSample <- gbutton("Resample")
    ## gWidgets for Plot
    reverseWL <- gcheckbox("Reverse Wavelength")
    bandwidthAdjust <- gslider(from=0,to=2,by=.01,value=1)
 
    ## create GUI window
    window <- gwindow("spike.filter - hyperSpec GUI")
    BigGroup <- ggroup(cont=window, size=c(75*3,75*2))
    group <- ggroup(horizontal=FALSE, container=BigGroup, size=c(75*3,75*2))
    group2 <- ggroup(horizontal=FALSE, container=BigGroup, size=c(75*3,75*2))
    group3 <- gpanedgroup(container=group2, size=c(75*3,75*2))
    
    
    index <- 1;
    updatePlots <- function(...) {
      
      par(mar=c(1,1,1,1))
    
    dev.set(ggmain); hist(rnorm(100))
    dev.set(ggsub1); hist(rnorm(100))
    dev.set(ggsub2); hist(rnorm(100))
    
      spc = cartilage[1:100]
      spikiness = scores[1:100, ]
       npts = 10; nspc = 1; save.tmp = 20; use.tmp = FALSE; ispikes = NULL; iispikes = NULL
  ## TODO: better move the first part to spike suspicio
  wavelength <- spc@wavelength
  spc <- spc@data$spc
  gc ()
  
  dim <- dim (spikiness) 
  dim(spikiness)  <- NULL # make vector
  
  if (is.null (ispikes))
    ispikes <- order (spikiness, na.last = TRUE, decreasing = TRUE)

  if (is.null (iispikes))
    iispikes <- order (ispikes)
  
  start.i = 1

  save.i <- 1
  
  for (i in start.i : 2){
    cat ("Spike suspicion: ", i, "\n" )

    if (is.na (ispikes[i]))
      next

    save.i <- save.i + 1

    ind <- vec2array (ispikes [i], dim = dim)
    cat ("   Spectrum: ", ind[1], "\n")
    k <- ind[1] + (-nspc : nspc) # suspicious spectrum plus the spectra around
    k <- k [k > 0]
    k <- k [k <= nrow (spc)]
    isna <- apply (spc[k,,drop = FALSE], 1, function (x) all (is.na (x)))
    k <- k[! isna]
dev.set(ggmain)#########################
    plot (wavelength, spc[ind[1],], ylim = range (spc[k,], na.rm = TRUE), type = "n")
    for (l in k)
      lines (wavelength, spc[l,], col = if (l == ind[1]) "blue" else "black")
    points (wavelength[ind[2]], spc[ind[1], ind[2]], col = "red", pch = 20)
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
dev.set(ggsub1)#################################
   plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = c (yl, yu), type = "n", ann=F)
    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20,
             type = "p",
             cex = 0.5,
             col = if (l == ind[1]) "blue" else "black")
    lines (wavelength[j], spc[ind [1],j], col = "blue")        
    points (wavelength[j], spc[ind[1], j], 
          col = "blue", pch = 20, type = "p"
          )

   y <- range (spc[k,j,drop = FALSE], na.rm = TRUE)
dev.set(ggsub2)##############################
   plot (wavelength[j], spc[ind[1],j], xlim = x, ylim = y, type = "n", ann=F)

    for (l in k)
      lines (wavelength[j], spc[l,j], pch = 20, type = "p", cex = 0.5,
          col = if (l == ind[1]) "blue" else "black")
          
    points (wavelength[j], spc[ind[1], j], 
          col = "blue", pch = 20, type = "p"
          )

    x <- rep (x[2], 3) 
    y <- (10:8)/10 * (y[2] - y [1]) + y[1]

    points (x, y, pch = 20, cex = 1, col = c("black", "#008000", "red"))
    text (x, y, labels = c("end", "spc OK", "bad spc"), pos = 2,
          col = c("black", "#008000", "red"))

    pts <- identify (c(wavelength[        j], x),
                     c(spc  [ind[1], j], y))

    if (length (pts) == 0)
      next

    if (max (pts) > length (j)){
      pts <- max (pts) - length (j)

      if (pts == 1) break

      # pts == 2 || pts == 3
      j <- array2vec (matrix (c(rep (ind[1], dim [2]),
                                1 : dim[2]),
                              ncol = 2),
                      dim)
      ispikes [iispikes[j]] <- NA

      if (pts == 3) # bad spectrum
        spc[ind[1], ] <- NA

      next
    }
      
    pts <- j [pts]
    #pts <- pts
    
    cat ("   Spike points: ", pts, "\n")
  
    spc[ind[1], pts] <- NA 
    
    pts <- array2vec (matrix (c (rep (ind[1], length (pts)), pts), ncol = 2), 
                      dim = dim)
    
    cat ("   Spikiness: ", spikiness [pts], "\n")
    ispikes [iispikes[pts]] <- NA     # do not look at this spike again

  }


      # check index, use that data
      dev.set(ggmain)
      plot(x,y)
      dev.set(ggsub1)
      hist(x)
      dev.set(ggsub2)
      hist(y)
    }
    nextPlot <- function() {
      index <<- index+1
    }
                               
    #tmp <- gframe("Sample size", container=group)
    #add(tmp,sampleSize)
    #add(tmp,reSample)
    
    #lapply(names(getStockIcons()), function(x) add(group, gbutton(x
    #forward, dismiss, ok
                               
    #add(gframe("Spc Number", container=group),bandwidthAdjust,
    #    expand=T)

    #add(gframe("Plotting options", container=group),reverseWL)
    #addStockIcons(
    #  c("next","drop","done"),
    #  c("/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/forward.gif",
    #    "/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/dismiss.gif",
    #    "/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/ok.gif"))
    #iconNames <- c("done")
    #iconFiles <- c("/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/ok.gif")
    #addStockIcons(iconNames, iconFiles)
#    add(group, glabel("<b>Spike suspicion</b>: 1<br />
#                      <b>Spectrum</b>: 67<br />
#                      (1560.94, 2219.1)<br />
#                      <b>Spike points</b>:  479 481<br />
#                      <b>Spikiness</b>:  0.06663623 -1.423397",
#                      markup = TRUE))

    qparacetamol <- qdata(list('wavelength'=paracetamol@wavelength,'spc'=as.numeric(paracetamol@data$spc)))
    x<-rnorm(100)
    y<-pnorm(x)
    add(group3, ggraphics())
    ggmain <- dev.cur()
    group4 <- ggroup(horizontal=FALSE, container=group3)
    add(group4, ggraphics())
    ggsub1 <- dev.cur()
    ggssub2 <- ggraphics()
    add(group4, ggssub2)
    ggsub2 <- dev.cur()
    tmp <- gframe("Spikes", container=group2)
    add(tmp, gbutton("Next Suspicion", handler=updatePlots))
    add(tmp,gimage('forward'))
    add(tmp, gbutton("Good Spectrum"))
    add(tmp,gimage('forward'))
    add(tmp, gbutton("Bad Spectrum"))
    add(tmp,gimage('dismiss'))
    add(tmp, gbutton("Done"))
    add(tmp,gimage('ok'))
    size(group3) <- c(75*10, 75*6)
    #size(ggsub2) <- c(75*4, 75*3)
    
    status <- gstatusbar("Suspicion 1 of 100", container=window)
    
    
    #add(group2, glabel("<b>Spike suspicion</b>: 1<br />
    #                  <b>Spectrum</b>: 67<br />
    #                  (1560.94, 2219.1)<br />
    #                  <b>Spike points</b>:  479 481<br />
    #                  <b>Spikiness</b>:  0.06663623 -1.423397",
    #                  markup = TRUE))
    
    ## create mutaframe for spikes and return..
    spikes <- as.mutaframe(list('selected'=rep(FALSE,length(x))))
}

spikes.gui()