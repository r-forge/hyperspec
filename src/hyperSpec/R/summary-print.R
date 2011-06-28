##’ Convert a hyerSpec object to character strings for Display
##’ \code{print}, \code{show}, and \code{summary} show the result of
##’ \code{as.character}.
##’ 
##’ \code{print}, \code{show}, and \code{summary} differ only in the defaults.
##’ \code{print} shows the overview giving the first and last values of each
##’ data column (fastest). \code{show} displays the range of values instead,
##’ and \code{summary} displays the logbook as well.
##’ 
##’ @name show
##’ @aliases show,hyperSpec-method print,hyperSpec-method
##’   summary,hyperSpec-method as.character,hyperSpec-method
##’ @docType methods
##’ @param x,object a \code{hyperSpec} object
##’ @param digits number of digits handed over to \code{format}
##’ @param range should the values be indicated as range rather then first and
##’   last elements?
##’ @param max.print maximum number of elements to be printed (of a variable)
##’ @param shorten.to if a vector is longer than \code{max.print}, only the
##’   first \code{shorten.to[1]} and the last \code{shorten.to[2]} elements are
##’   printed
##’ @param log should the log be printed?
##’ @param \dots \code{print} hands any arguments to \code{as.character}
##’ @return \code{as.character} returns a character vector fit to be printed by
##’   \code{cat} with \code{sep = "\n"}.
##’ 
##’ \code{print} invisibly returns \code{x} after printing, \code{show} returns
##’   an invisible \code{NULL}.
##’ @seealso \code{\link[base]{print}}, \code{\link[methods]{show}}, and
##’   \code{\link[base]{as.character}}
##’ @keywords methods print
##’ @examples
##’ 
##’ chondro
##’ 
##’ show (chondro)
##’ 
##’ summary (chondro)
##’ 
##’ print (chondro, log = TRUE)
##’ print (chondro, range = TRUE)
##’ 
##’ logbook (chondro)
##’
setMethod ("summary", "hyperSpec", function (object, log = TRUE, ...){
  print (object, log = log, ...)
})
