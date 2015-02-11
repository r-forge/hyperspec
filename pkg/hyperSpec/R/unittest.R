##' hyperSpec unit tests
##' Run the unit tests and display the results.
##'
##' @rdname unittests
##' @return \code{TRUE} if all tests are passed successfully. If a test
##'   fails, \code{hy.unittest} stops with an error.
##' @author C. Beleites
##' @seealso \code{\link[svUnit]{svUnit}}
##' @keywords programming utilities
##' @export
##' @examples
##' 
##'   hy.unittest ()
##' 
##' @importFrom svUnit is.test clearLog runTest Log errorLog stats
hy.unittest <- function (){
  tests <- unlist (eapply (env = getNamespace ("hyperSpec"), FUN = is.test, all.names = TRUE))
  tests <- names (tests [tests])
  tests <- sapply (tests, get, envir = getNamespace ("hyperSpec"))

  clearLog ()
  warnlevel <- options()$warn
  options (warn = 0)
  for (t in seq_along (tests))
    runTest (tests [[t]], names (tests) [t])
  options (warn = warnlevel)

  if (interactive ())
    print (stats (Log()))
  else
    print (stats (Log ())[,c ("kind", "msg")])

  errorLog (summarize = FALSE)
  invisible (TRUE)
}
