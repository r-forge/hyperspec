---
layout: post
title: Witec ASCII file import
tags: fileio news Witec
---

Witec supports two types of ASCII export, which now can both be read by `hyperSpec`:

- storing x (wavelength) and y (intensity) information in separate files ("dat"), and
- storing the wavelengths as the first column and the intensities as the following columns ("txt").

**ATTENTION:** the ASCII import functions have been renamed:

<!-- end excerpt -->
- `.txt` (the one-file format) now imported by `scan.txt.Witec`, while
- `.dat` (the two files format) is read by `scan.dat.Witec`.

For examples, see `vignette ("fileio")`.
