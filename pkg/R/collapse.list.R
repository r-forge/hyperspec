##' collapse several hyperSpec objects into one object
##'
##' The spectra from all objects will be put into one object.
##' The resulting object has all wavelengths that occur in the input objects.
##' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
##' Extra data is combined in the same manner.
##' 
##' @author C. Beleites
##' @export
##' @param ... hyperSpec objects to be collapsed into one object. Instead of giving several arguments, a list with all objects to be collapsed may be given.
##' @return a hyperSpec object
##' @examples
##'  A <- new ("hyperSpec", spc = c(0, 1, .2), wavelength = c (1, 3, 7))
##'  B <- new ("hyperSpec", spc = runif (4), wavelength = c (1, 2, 4, 5))
##'  plotspc (A)
##'  plotspc (B, add = TRUE, col = "darkgray")
##'  plotspc (collapse (A, B), add = TRUE, col = c("red", "blue"),
##'           lines.args = list (type = "p", pch = 20))
##' 
##' A$.
##' B$.
##' collapse (A,B)$.

## TODO: try dispatch on dots?
rbind.fill.hyperSpec <- function (..., orderwl = TRUE, short = "collapse", user = NULL, date = NULL){
  dots <- list (...)

  ## check the arguments
  lapply (dots, .is.hy)
  lapply (dots, validObject)

  logs <- list()

  ## calculate the common wavelength axis
  wl <- unique (unlist (lapply (dots, wl)))

  ## preallocate the new spectra matrix
  pos <- sapply (dots, nrow)
  spc <- matrix (NA_real_, nrow = sum (pos), ncol = length (wl))

  ## make an index vector for the row positions
  pos <- c (0, cumsum (pos))

  ## fill in the matrix and record for the logbook
  label <- list ()
  for (i in seq_along (dots)){
    logs [[i]] <- .paste.row (dots [[i]])
    spc [(pos [i] + 1) : pos [i + 1], match (dots[[i]]@wavelength, wl)] <- dots[[i]]@data$spc
    label <- modifyList (label, dots [[i]]@label)
    ## TODO: warn if different labels!
  }

  ## post-process the log entry
  names (logs) <- names (dots)
  logs <- c (logs, list (orderwl = orderwl))

  ## extra data
  ## this is not only nice but also important: otherwise rbind.fill does not
  ## produce rows for objects without extra data columns.
  dots <- mapply (function (x, y) {x$.object = y; x}, dots, seq_along (dots))
  dots <- lapply (dots, function (x) x$..)
  dots <- rbind.fill (dots)

  ## make a new hyperSpec object
  x <- new ("hyperSpec", spc = spc, wavelength = wl, data = dots, label = label,
            log = list (short = short, long = logs, user = user, date = date))
  
  if (orderwl)
    x <- orderwl (x)

  x
}

