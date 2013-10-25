--- 
layout: post
title: 'Extended binary arithmetic operators'
tags: news 
---

`hyperSpec`'s binary arithmetic operators (`+`, `-`, `*`, `/`) now working for calculations on
spectra matrix plus row or column-vector objects as well, so that many calls to `sweep` can now be
more easily expressed, e.g.

Centering:
{% highlight rconsole %}
> plot (flu - colMeans (flu))
{% endhighlight %}

Normalization:
{% highlight rconsole %}
> plot (flu / rowMeans (flu))
{% endhighlight %}
<!-- end excerpt -->
