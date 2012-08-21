##' Run the unit tests
##'
##' Run the unit tests attached to the functions via \link[svUnit]{svUnit} 
##' @return invisibly \code{TRUE} if the tests pass, \code{NA} if \link[svUnit]{svUnit} is not
##' available. Stops if errors are encountered.
##' @author Claudia Beleites
##' @seealso  \link[svUnit]{svUnit} 
##' @export 
cbmodels.unittest <- function (){
  if (! require (svUnit)){
    warning ("svUnit required to run the unit tests.")
    return (NA)
  }

  tests <- unlist (eapply (env = getNamespace ("cbmodels"), FUN = is.test, all.names = TRUE))
  tests <- names (tests [tests])

  tests <- sapply (tests, get, envir = getNamespace ("cbmodels"))
  tests <- sapply (tests, attr, "test")

  clearLog ()
  
  warnlevel <- options()$warn
  options (warn = 0)
  for (t in seq_along (tests))
    runTest (tests [[t]], names (tests) [t])
  options (warn = warnlevel)
  print (stats (Log()))

  errorLog (summarize = FALSE)
  invisible (TRUE)
}


