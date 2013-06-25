---
layout: default
title: Download & Installation
---
<h1>Download &amp; Installation</h1>

There are several ways to obtain <tt>hyperSpec<tt>. 

<ul>
<li> <a href= "#cran">Install stable version</a> </li>
<li> <a href= "#rforge">Install nightly build</a> </li>
<li> <a href= "#tgz">Install from .tar.gz or .zip archive</a> </li>
<li> <a href= "#svn">Clone svn repository</a> </li>
</ul>

<div class = "post">

<h2 id="cran">Install stable version</h2>

<p>Inside R type:

{% highlight rconsole %}
> install.packages ("hyperSpec")
{% endhighlight %}

to install the stable version from <a href="http://cran.r-project.org/web/packages/hyperspec/index.html">CRAN</a>.</p>

&raquo; <a href="#download__installation">top</a>
</div>

<div class = "post">
<h2 id="rforge">Install nightly build</h2>
To install the nightly build (development version) from <a href="https://r-forge.r-project.org/R/?group_id=366">R-Forge</a>, inside R type:

{% highlight rconsole %}
> install.packages ("hyperSpec", repos="http://r-forge.r-project.org")
{% endhighlight %}

Nightly builds of Windows binaries are sometimes a few days delayed.

<br/>&raquo; <a href="#download__installation">top</a>

</div>

<div class = "post">
<h2 id="tgz">Install from .tar.gz or .zip archive</h2>
<h3>Download <tt>hyperSpec</tt> prebuilt on my computer</h3>
<ul>
<li> <a href="/blob/hyperSpec-prebuilt.tar.gz">prebuilt .tar.gz source
		(<?php echo date ("Y-M-d", filectime ("/blob/hyperSpec-prebuilt.tar.gz")) ?>)</a></li>
<li> Windows users will probably need the <a href="/blob/hyperSpec-prebuilt.zip">compiled .zip archive
		(<?php echo date ("Y-M-d", filectime ("/blob/hyperSpec-prebuilt.zip")) ?>)</a><br/></li>
<li> Versions for r-devel: <a href="/blob/hyperSpec-prebuilt-devel.tar.gz">tar.gz
		(<?php echo date ("Y-M-d", filectime ("/blob/hyperSpec-prebuilt-devel.tar.gz")) ?>)</a></li>
</ul>

<p>Source (.tar.gz) and Windows binary (.zip) packages can be installed by </p>

<h3>command line:</h3>
<p>
{% highlight console %}
$ R CMD INSTALL filename.tar.gz
{% endhighlight %}
</p>

<h3>in <a href="http://www.rstudio.org/">RStudio</a></h3>
<p>
click "Install Packages" in the package tab and then select "Install from: Package Archive"
<br/>

<i>Do not unpack the archive.</i>
</p>


&raquo; <a href="#download__installation">top</a>
</div>

<div class = "post">
<h2 id ="svn">Clone svn repository</h2>
<h3><tt>svn</tt> users:</h3>

<p>checkout using your favourite <tt>svn</tt> client program, or  
{% highlight console %}
$ svn checkout svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg
{% endhighlight %}
</p>       

<h3><tt>git</tt> users:</h3>
<p>
<tt>git-svn</tt> is highly recommended:

{% highlight console %}
$ git svn clone  svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg \
> hyperSpec
{% endhighlight %}

<code>-rHEAD</code> makes a shallow copy retrieving only the most recent revision.
</p>
<h3>Install from repository clone</h3>
<p>
Build and install the package:  
{% highlight console %}
$ R CMD build pkg/
$ R CMD INSTALL hyperSpec_0.xx-yyyymmdd.tar.gz
{% endhighlight %}
</p>
&raquo; <a href="#download__installation">top</a>
</div>
<!---
<div class = "post">
<h2><a name=""></a></h2>
<p>

</p>&raquo; <a href="#download__installation">top</a>
</div>
--->
