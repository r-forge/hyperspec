---
title: Documentation
layout: default
---
# Documentation

`hyperSpec`'s main documentation comes in the form of vignettes. 

These are automatically installed with `hyperSpec` and can be opened with

{% highlight rconsole %}
> vignette ("introduction")
{% endhighlight %}

{% highlight rconsole %}
> vignette (package = "hyperSpec")
{% endhighlight %}
yields a list of available vignettes, and they are also linked in the help pages.

- `"introduction"` - [user manual](blob/introduction.pdf) written from a spectroscopist's point of view.

- `"fileio"` - Detailed discussion of [import and export of spectra files](blob/fileio.pdf),  
   the [zipped directory containing all data](blob/fileio.zip) is needed to reproduce the vignette.

- `"plotting"` - [graphical manual](blob/plotting.pdf)  
  example plots together with the code to produce the plots

- `"flu"` - Example work-flow: [calibration of quinine fluorescence emission](blob/flu.pdf)  
  how to program import functions for other file formats, and how to set up a calibration.

- `"laser'` - Example work-flow: [Unstable Laser Emission](blob/laser.pdf)  
  working with time series, conversion of the spectral abscissa ("wavelength axis")

- `"chondro"` - Example work-flow: [Raman map of chondrocytes in cartilage](blob/chondro.pdf)  
	principal component analysis, cluster analysis, working with spectral maps/images.   
	The [zipped directory containing all data](blob/chondro.zip) is needed to reproduce
	the vignette.

- `"baseline"` - A more technical [explanation of baseline fitting techniques](blob/baseline.pdf)

In the standard `hyperSpec` installation, .pdf files and source are already available in the
documentation directory, with the exception of the the chondrocyte and fileio raw data (see above).
