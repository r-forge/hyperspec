.onLoad <- function (libname, pkgname){
  require (lattice)
  require (utils)
  require (plotrix)

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
       'The project is hosted on http://r-forge.r-project.org/projects/hyperspec/\n\n',
       sep = "")
}

