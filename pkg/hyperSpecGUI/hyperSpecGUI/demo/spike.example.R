## Spike Demo
library(hyperSpecGUI)

cartilage
plot (cartilage)

## this preprocessing removes much of the signal, leaving mostly spikes
tmp <- sweep (cartilage, 1, median, `/`)
tmp <- sweep (tmp, 2, median, `-`)
scores <- spikefilter2d (spcmatrix = tmp [[]])

## now use the original spectra and the suspicions from the preprocessed
## spectra for the interactive spike filter
spikes <- spikes.interactive (cartilage, scores)
spikes

