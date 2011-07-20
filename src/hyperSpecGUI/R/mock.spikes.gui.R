library(gWidgets)
library(hyperSpec)
options("guiToolkit"="Qt")
 
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
    group3 <- ggroup(container=group2)
                               
    tmp <- gframe("Sample size", container=group)
    add(tmp,sampleSize)
    add(tmp,reSample)
    
    #lapply(names(getStockIcons()), function(x) add(group, gbutton(x
    #forward, dismiss, ok
                               
    add(gframe("Some slider", container=group),bandwidthAdjust,
        expand=T)

    add(gframe("Plotting options", container=group),reverseWL)
    #addStockIcons(
    #  c("next","drop","done"),
    #  c("/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/forward.gif",
    #    "/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/dismiss.gif",
    #    "/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/ok.gif"))
    #iconNames <- c("done")
    #iconFiles <- c("/home/sebastian/tmp/hyperspec/pkg/hyperSpecGUI/inst/images/ok.gif")
    #addStockIcons(iconNames, iconFiles)
    add(group, gtext("Spike suspicion: 1\n   Spectrum: 67\n[1] 1560.94\n[1] 2219.1\n  Spike points:  479 481\nSpikiness:  0.06663623 -1.423397"))

    qparacetamol <- qdata(list('wavelength'=paracetamol@wavelength,'spc'=as.numeric(paracetamol@data$spc)))
    add(group3, qscatter(wavelength,spc,qparacetamol))
    group4 <- ggroup(horizontal=FALSE, container=group3)
    add(group4, qscatter(wavelength,spc,qparacetamol))
    add(group4, qscatter(wavelength,spc,qparacetamol))
    tmp <- gframe("Spikes", container=group2)
    add(tmp, gbutton("Next Suspicion"))
    add(tmp,gimage('forward'))
    add(tmp, gbutton("Drop Spectrum"))
    add(tmp,gimage('dismiss'))
    add(tmp, gbutton("Done"))
    add(tmp,gimage('ok'))
}
