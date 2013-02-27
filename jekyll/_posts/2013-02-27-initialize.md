---
layout: post
title: Bugfix hyperSpec initialization
tags: news bugfix
---

`hyperSpec`s initialization method has been fixed that prevented creation of `hyperSpec` objects when the `spc=` argument to `new` was a data.frame.

<!-- end excerpt --> 

{% highlight rconsole %}
> spc <- as.data.frame (flu [[]])
> class (spc)
[1] "data.frame"
> new ("hyperSpec", spc = as.data.frame (flu [[]]))
hyperSpec object
   6 spectra
   1 data columns
   181 data points / spectrum
wavelength:  [integer] 1 2 ... 181 
data:  (6 rows x 1 columns)
   1. spc:  [matrix181] 27.15000 66.80133 ... 294.6495 
Warnmeldung:
In .local(.Object, ...) : NAs durch Umwandlung erzeugt
{% endhighlight %}

The warning is supposed to be there: `hyperSpec` tries to guess the wavelengths from the colnames of
the `data.frame`, which fails. The resulting `hyperSpec` object therefore gets dummy wavelengths that
just count up from 1 to 181 (`ncol`).

The prebuilt [source package](/blob/hyperSpec-prebuilt.tar.gz)
and [windows binary](/blob/hyperSpec-prebuilt.zip) have been
renewed.
