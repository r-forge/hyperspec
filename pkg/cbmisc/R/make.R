#' Interface to `make` for package building
#' 
#' \code{make} calls make for the specified target. 
#' 
#' @param target Makefile target, use \code{make_geteasytargets} to get a list of possible targets.
#' @param path path where makefile resides
#' @param makefile the Makefile to be used
#' @export 
#' @examples
#' \dontrun{
#' make ("build")
#' }
make <- function (target = "all", path = "./", makefile = "Makefile"){
  
  oldwd <- getwd ()
  on.exit (setwd (oldwd))
  setwd (path)
   
  if (! file.exists (makefile))
    stop ("Makefile (", getwd (), "/", makefile, ") not found.")
  
  system2 ("make", args = paste ("-f", makefile, target))
}

#' 
#' \code{make_geteasytargets} extracts "easy" targets from the makefile, i.e.
#' non-complex (no "%") targets that are also not filenames.
#' 
#' @export
#' @rdname make
#' @examples
#' 
#' \dontrun{
#' make_geteasytargets ()
#' }
make_geteasytargets <- function (path = "./", makefile = "Makefile"){
  makefile <- readLines(paste0 (path, "/", makefile))
  
  targets <- makefile [grepl (":", makefile)]
  targets <- targets [! grepl ("^([[:space:]]|#)", targets)]
  targets <- targets [! grepl (":=", targets)]
  targets <- gsub (":.*$", "", targets)
  
  ## now prune everything that is not an "easy" target
  targets <- targets [! grepl ("%", targets)] # no complex targets
  targets <- targets [! grepl ("^[.]", targets)] # no hidden targets
  targets <- targets [! grepl ("/", targets)] # no targets in subdirectories
  
  targets [! file.exists (targets)]
}

