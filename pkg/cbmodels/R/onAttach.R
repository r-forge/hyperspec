.onAttach <- function (libname, pkgname = "cbmodels"){
  desc <- utils::packageDescription("cbmodels")
  vers <- paste("V. ", desc$Version)

  packageStartupMessage ("Package ",  desc$Package, ", version ", desc$Version, "\n\n",
       "If you use this package please cite it appropriately.\n",
       "   citation(\"", pkgname, "\")\nwill give you the correct reference.", "\n\n",
       "The project homepage is http://hyperspec.r-forge.r-project.org\n\n",
       sep = "")

#  unlockBinding("kernelpls.fit.original", asNamespace("hyperSpec"))
#  assignInNamespace ("kernelpls.fit.original", pls:::kernelpls.fit, "cbmodels")
  packageStartupMessage("Replacing pls::kernelpls.fit with extended function.")
  assignInNamespace ("kernelpls.fit", kernelpls.fit, "pls")
}
