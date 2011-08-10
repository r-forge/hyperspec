## Spike Demo
library(hyperSpecGUI)

cartilage
plot (cartilage)

## this preprocessing removes much of the signal, leaving mostly spikes
tmp <- sweep (cartilage, 1, median, `/`)
tmp <- sweep (tmp, 2, median, `-`)
plot (tmp, "mat", col.regions = alois.palette ())
scores <- spikefilter2d (spcmatrix = tmp [[]])
suspicions <- make.suspicions (scores)
plot (suspicions[, "spikiness"])
## now use the original spectra and the suspicions from the preprocessed
## spectra for the interactive spike filter
spikes <- spikes.interactive (cartilage, )
spikes

