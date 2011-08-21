## Spike Demo
library(hyperSpecGUI)

cartilage
plot (cartilage)

## this preprocessing removes much of the signal, leaving mostly spikes
tmp <- sweep (cartilage, 1, median, `/`)
tmp <- sweep (tmp, 2, median, `-`)
plot (tmp, "mat", col.regions = alois.palette ())
scores <- spikefilter2d (spcmatrix = tmp [[]])

## this lets plotting continue in demo mode
oldask <- options ()$device.ask.default
on.exit (options (device.ask.default = oldask), add = TRUE)
options (device.ask.default = FALSE)

options("guiToolkit"="RGtk2")
suspicions <- make.suspicions (scores)
spikes <- spikes.interactive (cartilage, suspicions)
#spikes <- spikes.interactive (cartilage, scores)



#suspicions <- make.suspicions (scores)
##plot (suspicions[, "spikiness"])
#spikes.interactive.cb (cartilage, suspicions)
#spikes
