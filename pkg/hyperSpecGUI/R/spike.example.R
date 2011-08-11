## Spike Demo
library(hyperSpec)

setwd("~/tmp/hyperspec/dev")
load("cartilage-raw.RData")
source("spikefilter.R")
setwd("~/tmp/hyperspec/src/hyperSpecGUI/R")

tmp <- sweep(cartilage, 1, median, `/`)
tmp <- sweep(tmp, 2, median, `-`)
scores <- spikefilter2d(spcmatrix=tmp[[]])

#spikes <- spikes.interactive(cartilage[1:100], scores[1:100, ])

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