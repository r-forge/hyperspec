pkg.exists <- function (pkg = stop ("package name needed"), lib.loc = NULL){
  dir <- sapply (pkg, function (p) system.file (package = p, lib.loc = lib.loc))
  nzchar (dir) > 0L 
}

is.basepkg <- function (pkg){
  pkg.exists (pkg) && grepl ("^base$", packageDescription (pkg, fields = "Priority"))
}

pkg.or.base <- function (pkg){
  pkg [sapply (pkg, is.basepkg)] <- "base"
  
  pkg
}

##' retrieve citation object with fallback to saved citations.
##'
##' retrieves \code{citation (pkg)} if package pkg is available. Alternatively, a CITATION file can
##' be read.
##' @rdname bibliography
##' @param pkg 
##' @param saved.cit CITATION file on harddisk
##' @return citation
##' @author Claudia Beleites
##' @export 
citation.or.file <- function (pkg, saved.cit = sprintf ("%s.CITATION", pkg)){
  if (pkg.exists (pkg))
    citation (pkg)
  else if (file.exists (saved.cit))
    readCitationFile (file = saved.cit)
  else
    NULL
}

make.cite.keys <- function (pkg, entries){
  pkg <- pkg.or.base (pkg)

  if (! pkg.exists (pkg))
    return (pkg)
  
  if (missing (entries))
    entries <- citation.or.file (pkg)
  
  keys <- sapply (unclass (entries), attr, "key")
  
  noname <- which (sapply (keys, is.null))

  if (length (keys) == 1L && noname == 1L) {
    keys <- pkg
  } else {
    for (i in noname)
      keys [[i]] <- paste (pkg, i, sep = ".")
  }

  keys <- make.unique (unlist (keys))
  
  keys
}
  
##' citations with automatic BibTeX keys 
##'
##' @rdname bibliography
##' @param pkg package
##' @return citation
##' @author Claudia Beleites
##' @export
citation.with.key <- function (pkg = "base"){
  pkg <- pkg.or.base (pkg)

  tmp <- citation.or.file (pkg)
  
  keys <- make.cite.keys (pkg, tmp)

  for (entry in seq_along (tmp))
    tmp [entry]$"key" <- keys [[entry]]

  tmp
}

##' cite a package
##'
##' @rdname bibliography
##' @param pkg package
##' @param entries which \code{citation ()} entries to cite?
##' @param citefun which LaTeX command to use for the citiation.
##' @return character with \code{\\cite{pkg.1, pkg.2}} 
##' @author Claudia Beleites
##' @export
##' @examples
##' cite.pkg ("cbmisc")
cite.pkg <- function (pkg, entries, citefun = "cite"){
  paste ("\\\\", citefun, "{", paste (make.cite.keys (pkg, entries), collapse = ", "), "}", sep = "")
}
##' Produce BibTeX file including automatic keys.
##'
##' @param ... packages
##' @param file name of .bib file \code{NULL} suppresses writing of the file. \code{""} will output
##' to stdout.
##' @rdname bibliography
##' @return invisible bibentry list.stats
##' @author Claudia Beleites
##' @export
##' @examples
##' make.bib (file = "")
make.bib <- function (..., file = NULL) {
  pkg <- c (...)

  if (length (pkg) == 0L) {
    pkg <- loadedNamespaces()
 
    pkg <- unique (pkg.or.base (pkg))
  }
  
  l <- lapply (pkg, citation.with.key)
  l <- do.call ("c", l [! sapply (l, is.null)])

  if (!is.null (file))
    if (is.null (l))
      cat (NULL, file = file)           # touches file
    else
      cat (toBibtex (l), file = file, sep = "\n")
  
  invisible (l)
}
