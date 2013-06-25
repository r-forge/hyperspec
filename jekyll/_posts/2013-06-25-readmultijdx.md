---
layout: post
title: 'Import Multispectra JCAMP-DX files'
tags: fileio news JCAMP-DX
---

hyperSpec now imports multispectra JCAMP-DX files. 

<!-- end excerpt -->

`read.jdx` gains an argument `collapse.multi` that switches behaviour for multispectra files: if
`TRUE`, the spectra are `collapse`d into one hyperSpec object, otherwise a list of hyperSpec objects
is returned.


