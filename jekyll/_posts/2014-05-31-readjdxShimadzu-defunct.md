---
layout: post
title: <tt>read.jdx.Shimadzu</tt> superseeded by <tt>read.jdx</tt>
tags: news deprecated fileio
---

`read.jdx.Shimadzu` is no longer needed `read.jdx` is now available with more general functionality.

<!-- end excerpt --> 

Please update your code to: 

{% highlight r %}
read.jdx (filename, encoding, 
          header = list (xunits = expression (m/z), yunits = 'I / a.u.'))
{% endhighlight %}


  