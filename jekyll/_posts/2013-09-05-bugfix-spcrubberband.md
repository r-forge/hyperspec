---
layout: post
title: Bugfix <tt>spc.rubberband</tt> baseline fitting
tags: news bugfix
---

`spc.rubberband` was not working correctly for data where the baseline is /above/
(e.g. transmittance, reflectance).

This was corrected. The argument to switch to upper baselines is now `upper = TRUE` (instead of
`lower = FALSE` as it was before).

<!-- end excerpt --> 


The prebuilt [source package](/blob/hyperSpec-prebuilt.tar.gz)
and [windows binary](/blob/hyperSpec-prebuilt.zip) have been
renewed.
