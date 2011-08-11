## Spike Demo
library(hyperSpecGUI)

cartilage
plot (cartilage)

## this preprocessing removes much of the signal, leaving mostly spikes
tmp <- sweep (cartilage, 1, median, `/`)
tmp <- sweep (tmp, 2, median, `-`)
plot (tmp, "mat", col.regions = alois.palette ())
scores <- spikefilter2d (spcmatrix = tmp [[]])
spikes <- spikes.interactive (cartilage, scores)



suspicions <- make.suspicions (scores)
##plot (suspicions[, "spikiness"])
#spikes.interactive.cb (cartilage, suspicions)
#spikes
