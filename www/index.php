
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
<tr><td>
<a href="/"><img src="<?php echo $themeroot; ?>/images/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> The package is now publicly accessible from the SVN repository. </p>
<h2>Installation</h2>
<p>To install the latest nightly build, type in R: <pre>install.packages("hyperSpec",repos="http://R-Forge.R-project.org")</pre></p>
<h2>Vignettes</h2>
<p><pre>hyperSpec</pre> comes with some vignettes:</p>
<ul>
<li><a href="../Vignettes/intro/introduction.pdf">Introduction</a>: a user manual from aspectroscopist's point of view</li>
<li>Example work-flow: <a href="../Vignettes/flu/flu.pdf">calibration of quinine fluorescence emission</a></li>
<li>Example work-flow: <a href="../Vignettes/chondro/chondro.pdf">Cluster Analysis of a Raman Map of Chondrocytes in Cartilage</a><br/>
    the <a href="../Vignettes/chondro/chondro.txt">data file</a> is quite large (ca. 31 MB) and needs to be saved into the the same folder as the .Rnw file in order to reprduce the example. 
</li>
<li>A more technical <a href="../Vignettes/baselinebelow/baselinebelow.pdf">explanation of the baseline fitting technique used by <pre>spc.fit.poly.below</pre></a></li>
</ul> 
<p><pre>hyperSpec</pre> Both .pdf and source files (including the spectra files except <a href="../Vignettes/chondro/chondro.txt">chondro.txt</a>, which needs to be downloaded explicitly due to its size) are available, and are 
installed into the documentation directory by <pre>install.packages</pre>.</p>
<p>
Claudia Beleites<br/>
CENMAT, Materials and Natural Resources Dept.<br/> 
University of Trieste<br/>
e-mail: cbeleites at units.it
</p>


<p> You find the <strong>project summary page</strong> at <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
