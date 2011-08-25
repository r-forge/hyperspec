
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='http://r-forge.r-project.org/themes/rforge/';
$group_id=366;
echo '<?xml version="1.0" encoding="UTF-8"?>';
?>

<?php
	function TableOfContents($depth)
	/*AutoTOC function written by Alex Freeman
	* Released under CC-by-sa 3.0 license
	* http://www.10stripe.com/  */
	{
	$filename = __FILE__;
	//read in the file
	$file = fopen($filename,"r");
	$html_string = fread($file, filesize($filename));
	fclose($file);
 
	//get the headings down to the specified depth
	$pattern = '/<h[1-'.$depth.']*[^>]*>.*?<\/h[1-'.$depth.']>/';
	$whocares = preg_match_all($pattern,$html_string,$winners);
 
	//reformat the results to be more usable
	$heads = implode("\n",$winners[0]);
	$heads = str_replace('<a name="','<a href="#',$heads);
	$heads = str_replace('</a>','',$heads);
	$heads = preg_replace('/<h([1-'.$depth.'])[^>]*>/','<li class="toc$1">',$heads);
	$heads = preg_replace('/<\/h[1-'.$depth.']>/','</a></li>',$heads);
 
	//plug the results into appropriate HTML tags
	$contents = '<div id="toc"> 
	<p id="toc-header"><strong>Contents</strong></p>
	<ul>
	'.$heads.'
	</ul>
	</div>';
	echo $contents;
	}
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
		<p><tt>hyperSpec</tt> is a R package to handle hyperspectral data, i.e. spectra plus further
		information such as spatial information, time, concentrations, etc.</p>
		<p>Such data are frequently encountered in Raman, IR, NIR, UV/VIS, NMR, mass spectroscopy, AAS /
		AES, ...</p>
		<p><tt>hyperSpec</tt> acts as an interface for the convenient handling of the spectra.
		Chemometric data analysis is done by R and other R packages.</p>
  </td>
  <td align ="right"><a href="/"><img src="<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td>
</tr>
</table>

<?php TableOfContents (1); ?>

<p> The package is publicly accessible from the
  <a href="http://<?php echo $domain; ?>/scm/?group_id=<?php echo $group_id; ?>">SVN repository</a>.
</p>
<p> There is also a <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">R-forge project summary page</a>. </p>

<h1><a name="Installation">Installation</a></h1>
<h2><a name="Installation in R">Inside R</a></h2>
<ul>
	<li>To install the latest stable version from CRAN, type in R: 
		<pre>install.packages("hyperSpec")</pre>
	</li>
	<li>To install the latest nightly build (development version), type in R: 
		<pre>install.packages("hyperSpec",repos="http://R-Forge.R-project.org")</pre>
	</li>
</ul>

<h2><a name="Installation archive">From source or binary archives automatically built by r-forge (nightly build)</a></h2>
<ol>
<li><a href="http://<?php echo $domain; ?>/R/?group_id=<?php echo $group_id; ?>">Download the appropriate file</a>.</li>
<li>Install the package:<br/>
<tt>R CMD INSTALL <i>filename</i></tt></li>
</ol>
Please note that the automatic windows build on r-forge is often one or two days behind. Windows
users: do not unzip the archive.

<h2><a name="Installation svn">From svn source</a></h2>
<ol>
  <li>get an svn checkout using your favourite svn client program, or<br/>
    <tt>svn checkout svn://svn.r-forge.r-project.org/svnroot/hyperspec/pkg</tt>
  </li>
  <li> Install the package:<br/>
    <tt>R CMD INSTALL <i>pkg-directory</i></tt> <br/>
    where <tt><i>pkg-directory</i></tt> is the directory where the svn checkout went (default is ./pkg).
  </li>
</ol>

<h2><a name="Installation prebuilt">Via package built on my computer</a></h2>
<ol>
  <li>Download the <a href="hyperSpec-prebuilt.tar.gz">prebuilt .tar.gz source
		(<?php echo date ("Y-M-d", filectime ("hyperSpec-prebuilt.tar.gz")) ?>)</a>.<br/>
    Windows users will probably need the <a href="hyperSpec-prebuilt.zip">compiled .zip archive
		(<?php echo date ("Y-M-d", filectime ("hyperSpec-prebuilt.zip")) ?>)</a><br/>
		Versions for r-devel: <a href="hyperSpec-prebuilt-devel.tar.gz">tar.gz
		(<?php echo date ("Y-M-d", filectime ("hyperSpec-prebuilt-devel.tar.gz")) ?>)</a>
<!--		and <a href="hyperSpec-prebuilt-devel.zip">zip
		(<?php echo date ("Y-M-d", filectime ("hyperSpec-prebuilt-devel.zip")) ?>)</a> -->
  </li>
  <li>Install the package:<br/>
    <tt>R CMD INSTALL <i>filename</i></tt>
  </li>
</ol>
These may be a bit outdated (the date is pkg/DESCRIPTION), but they work if the nightly build fails.

<h1><a name="Documentation">Documentation</a></h1>

<ul>
	<li>The <a href="http://rwiki.sciviews.org/doku.php?id=packages:cran:hyperspec">Wiki-page</a> gives an overview and</li>
	<li>provdes also a list of <a	href="http://rwiki.sciviews.org/doku.php?id=packages:cran:hyperspec#faq">
		frequently asked questions</a>.
	<li><tt>hyperSpec</tt> comes with some pdf documentation (vignettes) in addition to the R help files:
		<ul>
			<li><a href="introduction.pdf">Introduction</a>: user manual written from a spectroscopist's point
				of view</li>
			<li><a href="fileio.pdf">fileio</a>: Detailed discussion of import and export of spectra files,<br/>
				the <a href="fileio.zip">zipped directory containing all data</a> is needed to reproduce the vignette.</li>
			<li><a href="plotting.pdf">plotting</a>: graphical manual: example plots together with the code to
				produce the plots</li>
			<li>Example work-flow: <a href="flu.pdf">calibration of quinine fluorescence emission</a>: how to
				program import functions for other file formats, and how to set up a calibration.</li>
			<li>Example work-flow: <a href="laser.pdf">Unstable Laser Emission</a>: working with time series,
				conversion of the spectral abscissa ("wavelength axis")</li>
			<li>Example work-flow: <a href="chondro.pdf"> Raman map of chondrocytes in cartilage</a>:
				principal component analysis, cluster analysis, working with spectral maps/images.<br/>
				the <a href="chondro.zip">zipped directory containing all data</a> is needed to reproduce
				the vignette.</li>
			<li>A more technical <a href="baseline.pdf">explanation of the baseline fitting technique
				used by <tt>spc.fit.poly.below</tt></a></li>
		</ul>
		<p> In the standard <tt>hyperSpec</tt> installation, .pdf files and source are already available in
		the documentation directory, with the exception of the the chondrocyte and fileio raw data (see
		above).</p>
	</li>
</ul>

<h1><a name="Help">Help, Suggestions, Feature Requests, and Bug Reports</a></h1>
<ul>
	<li>There is a <a href="http://r-forge.r-project.org/forum/forum.php?forum_id=1218">help forum</a>
    on <tt>hyperSpec</tt></li>
  <li>and a <a href="http://r-forge.r-project.org/forum/forum.php?forum_id=1217">discussion
		forum</a> where suggestions for further development of hyperSpec can be discussed.
  </li>
	<li>The very low traffic mailing list hyperSpec-commits is used to announce major bug fixes and
		new features. <a href="http://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/hyperspec-commits">Subscription</a>,
		<a href="http://lists.r-forge.r-project.org/pipermail/hyperspec-commits/">archives</a>,and
		<a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/swish.cgi?query=listname%3D%22hyperspec-commits%22+">search</a>
		are available.</li>	
  <li>The mailing list hyperSpec-help discusses questions on the use of hyperSpec:
		<a href="http://lists.r-forge.r-project.org/cgi-bin/mailman/listinfo/hyperspec-help">subscribe</a>,
		<a href="http://lists.r-forge.r-project.org/pipermail/hyperspec-help/">read the archives</a>, or
		<a href="https://lists.r-forge.r-project.org/cgi-bin/mailman/swish.cgi?query=listname%3D%22hyperspec-help%22+">search</a></li>
  <li>Trackers for <a href="http://r-forge.r-project.org/tracker/?atid=1504&group_id=366&func=browse">feature requests</a> and </li>
  <li><a href="http://r-forge.r-project.org/tracker/?atid=1501&group_id=366&func=browse">bugs</a> are also found on r-forge.</li>
</ul>

<h1><a name="Presentations about hyperSpec">Presentations about hyperSpec</a></h1>
<ul>
<?php include ('about-hyperSpec.html') ?>
<!--
  <li> C. Beleites, C. Krafft, J. Popp, and V. Sergo: 
	 <a href="Beleites-UseR-Flashpresentation.pdf">hyperSpec: working with spectroscopic data</a>
	 UseR! 2011</a></li>
  <li> <a href="Mellor-UseR-Poster"></a></li>
  <li> <a href="">Poster at ECSBM, Euroanalysis and FTIR-Workshop at RKI 2009</a></li>
-->
</ul>

<h1><a name="Publications using hyperSpec">Publications using hyperSpec</a></h1>
<ul> 
<?php include ('citing-hyperSpec.html') ?>
<!--
  <li> A. Bonifacio; C. Beleites; F. Vittur; E. Marsich; S. Semeraro; S. Paoletti & V. Sergo: <br/>
	<a href="http://dx.doi.org/10.1039/c0an00459f"> Chemical imaging of articular cartilage sections with Raman mapping, employing uni- and multi-variate methods for data analysis.</a>  <br/>
	 Analyst, 135, 3193-3204 (2010). <br/>
	 doi: http://dx.doi.org/10.1039/c0an00459f<br/> 
	 PMID: 20967391
  </li>
  <li>C. Beleites, K. Geiger, M. Kirsch, S. B. Sobottka, G. Schackert and R. Salzer: <br/>
	 <a href="http://www.springerlink.com/content/5020361378260270/">Raman spectroscopic grading of 
		astrocytoma tissues: using soft reference information.</a> <br/>
	 Anal Bioanal Chem, 400, 2801-2816 (2011). <br/>
	 doi: 10.1007/s00216-011-4985-4 <br/>
	 PMID: 21537917 <br/>
  </li>
-->
</ul>
<h1><a name="Contact">Contact</a></h1>
<p>
Claudia Beleites<br/>
Dept. of Spectroscopy and Microscopy,<br/> 
IPHT Jena<br/>
e-mail: claudia dot beleites at ipht minus jena dot de
</p>
</body>
</html>
