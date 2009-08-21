
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
	<td align ="center">
		 <h1>Welcome <tt>hyperSpec</tt>!</h1>
		 <p><tt>hyperSpec</tt> is a R package to handle hyperspectral data, i.e. spectra plus further information such as spatial information, time, concentrations, etc.</p>
		 <p>Such data are frequently encountered in Raman, IR, NIR, UV/VIS, NMR, mass spectroscopy, AAS / AES, ...</p>
		 <p><tt>hyperSpec</tt> acts as an interface for the convenient handling of the spectra. Chemometric data analysis is done by R and other R packages.</p>
	</td>
	<td align ="right"><a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td>
</tr>
</table>


<p> The package is now publicly accessible from the <a href="http://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/?root=hyperspec">SVN repository</a>. </p>

<h1>Installation</h1>
<h2>... from inside R</h2>
<p>To install the latest nightly build, type in R: 
<pre>install.packages("hyperSpec",repos="http://R-Forge.R-project.org")</pre>
</p>

<h2>... from svn source</h2>
<ol><li>
get an svn checkout using your favourite svn client program, or<br/>
<tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/hyperspec</tt></li>
<li> Install the package:<br/>
<tt>R CMD INSTALL <i>pkg-directory</i></tt> 
    where <tt><i>pkg-directory</i></tt> is the subdirectory called pkg of the directory where the svn checkout went.</li>
</ol>

<h2>... via source Package</h2>
<ol>
<li><a href="hyperSpec_0.7.tar.gz">Download</a> the prebuilt .tar.gz source package.<br/>
	 Windows users will probably need the <a href="hyperSpec_0.7.zip">compiled .zip package</a></li>
<li>Install the package:<br/>
<tt>R CMD INSTALL <i>filename</i></tt></li>
</ol>

These may be a bit outdated, but they work if the nightly build fails.


<h1>Vignettes</h1>
<p><tt>hyperSpec</tt> comes with some vignettes:</p>
<ul>
<li><a href="https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/Vignettes/intro/introduction.pdf?root=hyperspec">Introduction</a>: a user manual from aspectroscopist's point of view</li>
<li>Example work-flow: <a href="https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/Vignettes/flu/flu.pdf?root=hyperspec">calibration of quinine fluorescence emission</a></li>
<li>Example work-flow: <a href="https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/Vignettes/chondro/chondro.pdf?root=hyperspec">Cluster Analysis of a Raman Map of Chondrocytes in Cartilage</a><br/>
    the <a href="https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/Vignettes/chondro/chondro.txt?root=hyperspec">data file</a> is quite large (ca. 31 MB) and needs to be saved into the documentation folder  in order to reproduce the example. </li>
<li>A more technical <a href="https://r-forge.r-project.org/plugins/scmsvn/viewcvs.php/*checkout*/Vignettes/baseline/baselinebelow.pdf?root=hyperspec">explanation of the baseline fitting technique used by <tt>spc.fit.poly.below</tt></a></li>
</ul> 
<p> In the standard <tt>hyperSpec</tt> installation, .pdf files and source  are already available in the documentation directory, with the exception of the the chondrocyte raw data (see above).</p>


<h1>Useful Packages for Chemometric Data Analysis</h1>
<p>By no means complete, if you have suggestions, send me an email.</p>
<ul>
<li><tt>R.matlab</t>: Read &ampersand; write .mat files, run external calculations in Matlab</li>
<li><tt>pls</t>: Partial Least Squares regression</li>
</ul>

<p>
Claudia Beleites<br/>
CENMAT, Materials and Natural Resources Dept.<br/> 
University of Trieste<br/>
e-mail: cbeleites at units.it
</p>


<p> You find the <strong>project summary page</strong> <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
