
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr>
	<td><img src="hyperSpec-logo.png" alt = "hyperSpec logo"></td>
	<td align = "justify">
		 <h1 align = "center">Welcome to <tt>hyperSpec</tt>!</h1>
		 <p><tt>hyperSpec</tt> is a R package to handle hyperspectral data, i.e. spectra plus further information such as spatial information, time, concentrations, etc.</p>
		 <p>Such data are frequently encountered in Raman, IR, NIR, UV/VIS, NMR, mass spectroscopy, AAS / AES, ...</p>
		 <p><tt>hyperSpec</tt> acts as an interface for the convenient handling of the spectra. Chemometric data analysis is done by R and other R packages.</p>
	</td>
	<td align ="right"><a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td>
</tr>
</table>


<p> The package is now publicly accessible from the <a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/?root=hyperspec">SVN repository</a>. </p>
<p> You find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>
<h1><tt>hyperSpec</tt> in the <link href="http://socghop.appspot.com/gsoc/program/home/google/gsoc2010"/>Google Summer of Code 2010</link></h1>
Info about <tt>hyperSpec</tt>:
<ul>
<li>here</li>
<li><link href="http://rwiki.sciviews.org/doku.php?id=packages:cran:hyperspec"/>R Wiki</link></li>
<li><link href="http://rwiki.sciviews.org/doku.php?id=developers:projects:gsoc2010:hyperspec"/>Idea List for GSoC project</link></li>
<li><link href="http://www.r-project.org"/>R</link></li>
</ul>
Students please note:
<ul>
<li>For the official application to Google you need to solve one of the "stand alone" tasks of the <link href="http://rwiki.sciviews.org/doku.php?id=developers:projects:gsoc2010:hyperspec"/>Idea List for GSoC project</link><br />
You may contact me about the topic and outline of your solution.
</li>
<li>Application deadline is April 9th</li>
</ul>

<h1>Installation</h1>
<h2>... from inside R</h2>
<p>To install the latest stable version from CRAN, type in R: 
<pre>install.packages("hyperSpec")</pre>
</p>
<p>To install the latest nightly build (development version), type in R: 
<pre>install.packages("hyperSpec",repos="http://R-Forge.R-project.org")</pre>
</p>

<h2>... from source or binaries automatically built by r-forge (nightly build)</h2>
<ol>
<li><a href="http://r-forge.r-project.org/R/?group_id=366">Download</a> the appropriate file.</li>
<li>Install the package:<br/>
<tt>R CMD INSTALL <i>filename</i></tt></li>
</ol>

<h2>... from svn source</h2>
<ol><li>
get an svn checkout using your favourite svn client program, or<br/>
<tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg</tt></li>
<li> Install the package:<br/>
<tt>R CMD INSTALL <i>pkg-directory</i></tt> 
    where <tt><i>pkg-directory</i></tt> is the directory where the svn checkout went (default is ./.</li>
</ol>

<h2>... via package built on my computer</h2>
<ol>
<li><a href="hyperSpec-prebuilt.tar.gz">Download</a> the prebuilt .tar.gz source package.<br/>
	 Windows users will probably need the <a href="hyperSpec-prebuilt.zip">compiled .zip package</a></li>
<li>Install the package:<br/>
<tt>R CMD INSTALL <i>filename</i></tt></li>
</ol>

These may be a bit outdated, but they work if the nightly build fails.

<h1>Documentation</h1>
<p><tt>hyperSpec</tt> comes with some pdf documentation (vignettes) in addition to the R help files:</p>
<ul>
<li><a href="introduction.pdf">Introduction</a>: a user manual from aspectroscopist's point of view</li>
<li><a href="FileIO.pdf">FileIO</a>: Detailed discussion of import and export of spectra files,<br/>
the <a href="FileIO.zip">zipped directory containing all data</a> is needed to reproduce the vignette.
</li>
<li>Example work-flow: <a href="flu.pdf">calibration of quinine fluorescence emission</a>: how to program import functions for other file formats, and how to set up a calibration.</li>
<li>Example work-flow: <a href="laser.pdf">Unstable Laser Emission</a>: working with time series, conversion of the spectral abscissa ("wavelength axis")</li>
<li>Example work-flow: <a href="chondrocytes.pdf"> Raman map of chondrocytes in cartilage</a>: principal component analysis, cluster analysis, working with spectral maps/images.<br/>
    the <a href="chondrocytes.zip">zipped directory containing all data</a> is needed to reproduce the vignette.</li>
<li>A more technical <a href="baselinebelow.pdf">explanation of the baseline fitting technique used by <tt>spc.fit.poly.below</tt></a></li>
</ul> 
<p> In the standard <tt>hyperSpec</tt> installation, .pdf files and source  are already available in the documentation directory, with the exception of the the chondrocyte raw data (see above).</p>

<h1>Help, Suggestions, Feature Requests, and Bugs</h1>
<ul>
<li>There is a <a href="http://r-forge.r-project.org/forum/forum.php?forum_id=1218">help forum</a> on <tt>hyperSpec</tt></li>
<li>and a <a href="http://r-forge.r-project.org/forum/forum.php?forum_id=1217">discussion forum</a> where suggestions for further development of hyperSpec can be discussed.</li>
<li>The very low traffic mailing list hyperSpec-commits is used to announce major bug fixes and new features. <a href="http://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/hyperspec-commits">Subscription</a> and  <a href="http://lists.r-forge.r-project.org/pipermail/hyperspec-commits/">Archives</a> and <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/swish.cgi?query=listname%3D%22hyperspec-commits%22+">Searching</a> are available.</li>
<li>The mailing list hyperSpec-help discusses questions on the use of hyperSpec. <a href="http://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/hyperspec-help">Subscription</a> and  <a href="http://lists.r-forge.r-project.org/pipermail/hyperspec-help/">Archives</a> and <a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/swish.cgi?query=listname%3D%22hyperspec-help%22+">Searching</a> are available.</li>
<li>Trackers for <a href="http://r-forge.r-project.org/tracker/?atid=1504&group_id=366&func=browse">feature requests</a>, and </li>
<li><a href="http://r-forge.r-project.org/tracker/?atid=1501&group_id=366&func=browse">bugs</a> are also found on r-forge.</li>
</ul>

<h1>Other Useful Packages for Chemometric Data Analysis</h1>
<p>By no means complete, if you have suggestions, send me an email.</p>
<ul>
<li><tt>R.matlab</tt>: Read & write .mat files, run external calculations in Matlab</li>
<li><tt>pls</tt>: Partial Least Squares regression</li>
</ul>

<h1>Impressum</h1>
<p>
Claudia Beleites<br/>
CENMAT, Materials and Natural Resources Dept.<br/> 
University of Trieste<br/>
e-mail: cbeleites at units.it
</p>
</body>
</html>
