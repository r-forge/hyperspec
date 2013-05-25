---
layout: post
title: "Spectral smoothing: splines and Savitzky-Golay filters"
tags: news baseline smoothing
---
`hyperSpec` offers several ways for spectral smoothing:

<!-- end excerpt -->

- `loess` via function `spc.loess`
- binning of wavelenghts via function `spc.bin`
- smoothing splines via function `spc.smooth.spline`
- Several further filter, e.g. Savitzky-Golay smoothing, are available in package  `savgol`. This funciton can be applied directly to the `hyperSpec` object:  
  `apply (chondro, 1, sgolayfilt, n = 11)`
