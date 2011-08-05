## Spike Demo
library(hyperSpecGUI)

tmp <- sweep (cartilage, 1, median, `/`)
tmp <- sweep (tmp, 2, median, `-`)
scores <- spikefilter2d (spcmatrix = tmp [[]])

spikes <- spikes.interactive (cartilage, scores)
spikes

