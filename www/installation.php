<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" 
			 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <title>Download & Installation</title>
    <meta http-equiv="content-type" content="text/html; charset=UTF-8" />
    <meta http-equiv="content-language" content="en" />
    <link rel="stylesheet" href="/css/screen.css" type="text/css" media="screen" />
    <link rel="stylesheet" href="/css/col-blue.css" type="text/css" media="screen" />
    <link rel="stylesheet" href="/css/fonts.css" type="text/css" />
    <link rel="stylesheet" href="/css/pygments.css" type="text/css" />
    <link rel="stylesheet" href="/css/print.css" type="text/css" media="print" />
  </head>
  <body>
	 <div id="header">
		<img src="/images/hyperSpec-logo.png" style="vertical-align:middle; float:left" />
		<h1><tt>hyperSpec</tt></h1>
	 </div>
      <div id="nav">
	 <h2>Navigation</h2>
	 <ul>
		<li><a href="/index.html">Home</a></li>
		<li><a href="/news.html">NEWS</a></li>
		
		<li id="newsection"><a href="/installation.php">Download &amp; Installation</a></li>
		<li><a href="/license.html">License</a></li>
		
		<li id="newsection"><a href="/documentation.html">Documentation</a></li> 
		<li><a href="/literature.html">Literature about <tt>hyperSpec</tt></a></li>
		<li><a href="/help.html">Help</a></li> 
<!--		<li><a href="/applications.html">Applications/Use Cases</a></li> -->
    
    <li id="newsection"><a href="/devel.html">Development</a></li>
		<li><a href="/help.html">Feature Request</a></li> 
		<li><a href="/help.html">Report Bug/Issue</a></li> 
		
		<li id="newsection"><a href="/posts.html">All <tt>hyperSpec</tt> blog posts</a></li>
		
		<li id="newsection"><a href="/contact.html">Contact</a></li>
		<li><a href="/credits.html">Credits</a></li>
	 </ul>
  </div>
 
 	 		<div id="share">
		  <h2>Contact</h2>
		  Claudia Beleites<br/>
		  Chemometrische Beratung<br/> 
		  Södeler Weg 19<br/>
		  61200 Wölfersheim/Germany<br/>
		  e-mail: <tt><a href="mailto:chemometrie at beleites dot de?subject=[hyperSpec]">chemometrie at beleites dot de</a></tt>
		</div>
  
	 <div id="container">
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

<div class="highlight"><pre><code class="rconsole"><span class="gp">&gt; </span>install.packages <span class="p">(</span><span class="s">&quot;hyperSpec&quot;</span><span class="p">)</span>
</code></pre>
</div>


to install the stable version from <a href="http://cran.r-project.org/web/packages/hyperspec/index.html">CRAN</a>.</p>

&raquo; <a href="#download__installation">top</a>
</div>

<div class = "post">
<h2 id="rforge">Install nightly build</h2>
To install the nightly build (development version) from <a href="https://r-forge.r-project.org/R/?group_id=366">R-Forge</a>, inside R type:

<div class="highlight"><pre><code class="rconsole"><span class="gp">&gt; </span>install.packages <span class="p">(</span><span class="s">&quot;hyperSpec&quot;</span><span class="p">,</span> repos<span class="o">=</span><span class="s">&quot;http://r-forge.r-project.org&quot;</span><span class="p">)</span>
</code></pre>
</div>


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
<div class="highlight"><pre><code class="console"><span class="gp">$</span> R CMD INSTALL filename.tar.gz
</code></pre>
</div>

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
<div class="highlight"><pre><code class="console"><span class="gp">$</span> svn checkout svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg
</code></pre>
</div>

</p>       

<h3><tt>git</tt> users:</h3>
<p>
<tt>git-svn</tt> is highly recommended:

<div class="highlight"><pre><code class="console"><span class="gp">$</span> git svn clone  svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg <span class="se">\</span>
<span class="gp">&gt;</span> hyperSpec
</code></pre>
</div>


<code>-rHEAD</code> makes a shallow copy retrieving only the most recent revision.
</p>
<h3>Install from repository clone</h3>
<p>
Build and install the package:  
<div class="highlight"><pre><code class="console"><span class="gp">$</span> R CMD build pkg/
<span class="gp">$</span> R CMD INSTALL hyperSpec_0.xx-yyyymmdd.tar.gz
</code></pre>
</div>

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

	 </div>
  </body>
</html>
