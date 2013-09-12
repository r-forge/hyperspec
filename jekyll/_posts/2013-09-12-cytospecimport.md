--- 
layout: post
title: 'Import Cytospec .mat files: bugfix in `R.matlab` 2.0.4'
tags: fileio news R.matlab bugfix 
---

A recent update from `R.matlab` version 1.7.0 to 2.0.1 broke the import of Cytospec-saved Matlab
files due to different handling of Matlab structs.

Thanks to Henrik Bengtsson for taking immediately care of it, the import seamlessly works again wiht
version 2.0.4.

<!-- end excerpt -->

If you encounter this bug, you can install `R.matlab` directly from Henrik's site:

{% highlight rconsole %}
> source("http://www.braju.com/R/hbLite.R")
> hbLite("R.matlab")
{% endhighlight %}
