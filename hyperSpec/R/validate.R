hy.unittest <- function (){
  if (! require (svUnit)){
    warning ("svUnit required to run the unit tests.")
    return (NA)
  }

  tests <- unlist (eapply (env = getNamespace ("hyperSpec"), FUN = is.test, all.names = TRUE))
  tests <- names (tests [tests])

  tests <- sapply (tests, get, envir = getNamespace ("hyperSpec"))

  clearLog ()
  for (t in seq_along (tests))
    runTest (tests [[t]], names (tests) [t])
  print (stats (Log()))

  errorLog (summarize = FALSE)
  invisible (TRUE)
}
