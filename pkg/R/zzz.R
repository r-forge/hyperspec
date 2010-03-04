.onLoad <- function (libname, pkgname){
  require (lattice)
  require (utils)

  if (! require (plotrix)){
    cat ("hyperSpec will use its own replacement for plotrix' axis.break\n\n")
    axis.break <- .axis.break
  }

  if (! require (latticeExtra))
    cat ("package 'latticeExtra' is needed for Voronoi plots.\n\n")
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

