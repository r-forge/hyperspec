library(gWidgets)
library(hyperSpec)
library(cranvas)
library(plumbr) ## as.mutaframe
options("guiToolkit"="Qt")

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
    ###  +------window-----+
    ###  |+-----BigGrp----+|
    ###  ||+-grp-++-ggfx-+||
    ###  |||  gw ||      |||
    ###  |||  gw ||      |||
    ###  ||+-----++------+||
    ###  |+-----BigGrp----+|
    ###  +------window-----+
    window <- gwindow("spike.filter - hyperSpec GUI")
    BigGroup <- ggroup(cont=window)
    group <- ggroup(horizontal=FALSE, container=BigGroup)
    group2 <- ggroup(horizontal=FALSE, container=BigGroup)
    group3 <- gpanedgroup(container=group2)
                               
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
    add(group3, qscatter(wavelength,spc,qparacetamol))
    group4 <- ggroup(horizontal=FALSE, container=group3)
    add(group4, qscatter(wavelength,spc,qparacetamol))
    add(group4, qscatter(wavelength,spc,qparacetamol))
    tmp <- gframe("Spikes", container=group2)
    add(tmp, gbutton("Next Suspicion"))
    add(tmp,gimage('forward'))
    add(tmp, gbutton("Good Spectrum"))
    add(tmp,gimage('forward'))
    add(tmp, gbutton("Bad Spectrum"))
    add(tmp,gimage('dismiss'))
    add(tmp, gbutton("Done"))
    add(tmp,gimage('ok'))
    size(group3) <- c(75*9, 75*6)
    
    status <- gstatusbar("Suspicion 1 of 100", container=window)
    
    
    add(group2, glabel("<b>Spike suspicion</b>: 1<br />
                      <b>Spectrum</b>: 67<br />
                      (1560.94, 2219.1)<br />
                      <b>Spike points</b>:  479 481<br />
                      <b>Spikiness</b>:  0.06663623 -1.423397",
                      markup = TRUE))
    
    ## create mutaframe for spikes and return..
    spikes <- as.mutaframe()
}

spikes.gui()