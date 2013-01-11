--- 
layout: post
title: 'File import of [Cytospec]() .mat files'
tags: fileio, news, R.matlab
---

`hyperSpec` gains a new file import filter reading [Matlab](www.mathworks.com/products/matlab/) files
[Cytospec](www.cytospec.com). 

Package [`R.matlab`](http://cran.r-project.org/web/packages/R.matlab/index.html) is needed.

{% highlight rconsole %}
> read.cytomat ("mat.cytospec/cytospec.mat", blocks=TRUE)
[[1]]
hyperSpec object
   30 spectra
   5 data columns
   846 data points / spectrum
wavelength:  [numeric] 499.6426 502.8398 ... 3201.277 
data:  (30 rows x 5 columns)
   1. x:  [integer] 1 2 ... 6 
   2. y:  [integer] 1 1 ... 5 
   3. file:  [factor] mat.cytospec/cytospec.mat mat.cytospec/cytospec.mat ... mat.cytospec/cytospec.mat 
   4. block:  [integer] 1 1 ... 1 
   5. spc:  [matrix846] 1389.549 1304.182 ... 982.8965 

[[2]]
hyperSpec object
   30 spectra
   5 data columns
   846 data points / spectrum
wavelength:  [numeric] 499.6426 502.8398 ... 3201.277 
data:  (30 rows x 5 columns)
   1. x:  [integer] 1 2 ... 6 
   2. y:  [integer] 1 1 ... 5 
   3. file:  [factor] mat.cytospec/cytospec.mat mat.cytospec/cytospec.mat ... mat.cytospec/cytospec.mat 
   4. block:  [integer] 2 2 ... 2 
   5. spc:  [matrix846] 1.1857055 0.8888147 ... -1.164395 

{% endhighlight %}
