.onLoad <- function (libname, pkgname){
  require (lattice)
  require (utils)
  if (! require (plotrix, warn.conflicts = FALSE))
    warning ("hyperSpec will use its own replacement for plotrix' axis.break")  

  if (! require (latticeExtra))
    warning ("package 'latticeExtra' is needed for Voronoi plots.")
}

.onAttach <- function (libname, pkgname){
  desc <- utils::packageDescription("hyperSpec")
  vers <- paste("V. ", desc$Version)
  cat ("Package ",  desc$Package, ", version ", desc$Version, "\n\n",
       "To get started, try\n",
       '   vignette ("introduction", package = "hyperSpec")\n',
       '   package?hyperSpec \n',
       '   vignette (package = "hyperSpec")\n\n',
       "If you use this package please cite it appropriately.\n",
       "   citation(\"hyperSpec\")\nwill give you the correct reference.", "\n\n",
       "The project's homepage is http://hyperspec.r-forge.r-project.org\n\n",
       sep = "")
}

