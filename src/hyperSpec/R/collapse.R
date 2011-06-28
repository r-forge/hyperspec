##' collapse/bind several hyperSpec objects into one object
##'
##' The spectra from all objects will be put into one object.
##' The resulting object has all wavelengths that occur in the input objects.
##' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
##' Extra data is combined in the same manner.
##' 
##' @author C. Beleites
##' @title Collapse hyperSpec objects
##' @export
##' @param ... hyperSpec objects to be collapsed into one object. Instead of giving several
##' arguments, a list with all objects to be collapsed may be given.
##' @param short.log if \code{TRUE}, only the dimensions of the hyperSpec objects are logged
##' @param short handed over to \code{logentry}
##' @param user handed over to \code{logentry}
##' @param date handed over to \code{logentry}
##' @aliases collapse
##' @aliases collapse.hyperSpec
##' @aliases merge.hyperSpec
##' @aliases rbind.fill.hyperSpec
##' @seealso \code{\link[base]{merge}},  \code{\link[base]{rbind}},  \code{\link[plyr]{rbind.fill}}, 
##' @return a hyperSpec object
##' @keywords manip
##' @examples
##' barbiturates [1:3]
##' barb <- collapse (barbiturates [1:3])
##' barb
##' 
##' a <- barbiturates [[1]]
##' b <- barbiturates [[2]]
##' c <- barbiturates [[3]]
##' 
##' a
##' b
##' c
##' collapse (a, b, c)
##' 


##’ Collapse hyperSpec objects
##’ collapse/bind several hyperSpec objects into one object
##’ 
##’ The spectra from all objects will be put into one object. The resulting
##’ object has all wavelengths that occur in the input objects. Data points
##’ corresponding to wavelengths not in the original spectrum will be set to
##’ NA. Extra data is combined in the same manner.\code{collapse}: The spectra
##’ from all objects will be put into one object. The resulting object has all
##’ wavelengths that occur in the input objects. Data points corresponding to
##’ wavelengths not in the original spectrum will be set to NA. Extra data is
##’ combined in the same manner.
##’ 
##’ @aliases collapse collapse collapse.hyperSpec merge.hyperSpec
##’   rbind.fill.hyperSpec
##’ @param ... hyperSpec objects to be collapsed into one object. Instead of
##’   giving several arguments, a list with all objects to be collapsed may be
##’   given.
##’ @param short.log if \code{TRUE}, only the dimensions of the hyperSpec
##’   objects are logged
##’ @param short handed over to \code{logentry}
##’ @param user handed over to \code{logentry}
##’ @param date handed over to \code{logentry}
##’ @return a hyperSpec object\code{collapse}: a hyperSpec object
##’ @author C. Beleites
##’ @seealso \code{\link[base]{merge}}, \code{\link[base]{rbind}},
##’   \code{\link[plyr]{rbind.fill}},\code{\link[base]{merge}},
##’   \code{\link[base]{rbind}}, \code{\link[plyr]{rbind.fill}}
##’ @keywords manip manip
##’ @examples
##’ 
##’ barbiturates [1:3]
##’ barb <- collapse (barbiturates [1:3])
##’ barb
##’ 
##’ a <- barbiturates [[1]]
##’ b <- barbiturates [[2]]
##’ c <- barbiturates [[3]]
##’ 
##’ a
##’ b
##’ c
##’ collapse (a, b, c)
##’ 
collapse <- function (..., short.log = TRUE, short = "collapse", user = NULL, date = NULL){
  dots <- list (...)

  ## accept also a list of hyperSpec objects
  if (length (dots) == 1 && is.list (dots [[1]]))
    dots <- dots [[1]]

  ## check the arguments
  lapply (dots, chk.hy)
  lapply (dots, validObject)

  logs <- list()

  ## prepare log
  if (short.log)
    logs <- paste ("hyperSpec [",
                   unlist (lapply (dots, function (x) paste (dim (x), collapse = " x "))),
                   "]", sep = "")
  else
    logs <- unlist (lapply (dots, function (x) paste (as.character (x, range = FALSE), "\n", collapse = "")))

  ## prepare new labels


##’ Get and Set Labels of a hyperSpec Object
##’ \code{labels}
##’ 
##’ \code{value} may be a list or vector of labels giving the new label for
##’ each of the entries specified by \code{which}.
##’ 
##’ The names of the labels are the same as the colnames of the
##’ \code{data.frame}.  The label for the wavelength axis has the name
##’ \code{.wavelength}.
##’ 
##’ The labels should be given in a form ready for the text-drawing functions
##’ (see \code{\link[grDevices]{plotmath}}), e.g. as \code{expression} or a
##’ \code{character}.
##’ 
##’ @aliases labels,hyperSpec-method labels<-
##’ @param object a hyperSpec object
##’ @param which numeric or character to specify the label(s)
##’ @param drop if the result would be a list with only one element, should the
##’   element be returned instead?
##’ @param \dots ignored
##’ @param use.colnames should missing labels be replaced by column names of
##’   the extra data?
##’ @param short,user,date handed to \code{\link[hyperSpec]{logentry}}
##’ @param value the new label(s)
##’ @return \code{labels} returns a list of labels.  If \code{drop} is
##’   \code{TRUE} and the list contains only one element, the element is
##’   returned instead.
##’ 
##’ \code{labels<-} returns a \code{hyperSpec} object.
##’ @author C. Beleites
##’ @seealso \code{\link[base]{labels}}
##’ @examples
##’ 
##’ labels (chondro)
##’ labels (flu, "c") <- expression ("/" ("c", "mg / l"))
##’ 
  labels <- unlist (lapply (dots, slot, "label"))
  labels <- labels [unique (names (labels))]
  
  ## merge data & spectra matrices
  dots <- lapply (dots, .wl2cln)
  dots <- rbind.fill (lapply (dots, slot, "data"))
  wl <- as.numeric (colnames (dots$spc))

  ## make a new hyperSpec object
  x <- new ("hyperSpec", wavelength = wl, data = dots, labels = labels,
            log = list (short = short, long = logs, user = user, date = date))
  
  x
}

.wl2cln <- function (x){
  colnames (x@data$spc) <- formatC (x@wavelength, digits = 17)
  x
}

