###-----------------------------------------------------------------------------
###
### bind: cbind & rbind
###
###



##’ Binding hyperSpec Objects
##’ Two S3 functions \code{cbind.hyperSpec} and \code{rbind.hyperSpec} act as
##’ an interfaces to \code{cbind2} and \code{rbind2} because neither
##’ \code{\link[Matrix]{rBind}} and \code{\link[Matrix]{cBind}} nor S4 versions
##’ of \code{cbind} and \code{rbind} do work at the moment.
##’ 
##’ While it is now possible to do S4 despatch on \code{\dots{}}, defining such
##’ S4 methods for \code{cbind} and \code{rbind} breaks the binding of
##’ \code{Matrix} objects. Therefore, two S3 methods \code{rbind.hyperSpec} and
##’ \code{cbind.hyperSpec} are defined.
##’ 
##’ \code{rbind2} binds two \code{hyperSpec} objects by row. They need to have
##’ the same columns.
##’ 
##’ \code{cbind2} binds the spectral matrices of two \code{hyperSpec} objects
##’ by column. All columns besides \code{spc} with the same name in
##’ \code{x@data} and \code{y@data} must have the same elements.  Rows are
##’ ordered before checking.
##’ 
##’ \code{bind} does the common work for both column- and row-wise binding.
##’ 
##’ @aliases bind cbind.hyperSpec rbind.hyperSpec
##’   cbind2,hyperSpec,hyperSpec-method rbind2,hyperSpec,hyperSpec-method
##’   cbind2,hyperSpec,missing-method rbind2,hyperSpec,missing-method
##’ @param \dots The \code{hyperSpec} objects to be combined.
##’ 
##’ Alternatively, \emph{one} list of \code{hyperSpec} objects can be given to
##’   \code{bind}.
##’ @param deparse.level ignored.
##’ @param short,user,date for the log
##’ @param x,y \code{hyperSpec} objects
##’ @param direction "r" or "c" to bind rows or columns
##’ @return a \code{hyperSpec} object, possibly with different row order (for
##’   \code{bind ("c", \dots{})} and \code{cbind2}).
##’ @note You might have to make sure that the objects either all have or all
##’   do not have rownames and/or colnames.
##’ @author C. Beleites
##’ @seealso \code{\link[Matrix]{rBind}}, \code{\link[Matrix]{cBind}}
##’   \code{\link[methods]{rbind2}}, \code{\link[methods]{cbind2}}
##’   \code{\link[base]{rbind}}, \code{\link[base]{cbind}}
##’ @keywords methods manip
##’ @examples
##’ 
##’ chondro
##’ bind ("r", chondro, chondro)
##’ rbind (chondro, chondro)
##’ cbind (chondro, chondro)
##’ bind ("r", list (chondro, chondro, chondro))
##’ 
##’ 
##’ x <- chondro[,, 600 : 605]
##’ x$a <- 1
##’ x@data <- x@data[, sample (ncol (x), ncol (x))] # reorder columns
##’ 
##’ y <- chondro [nrow (chondro) : 1,, 1730 : 1750] # reorder rows
##’ y$b <- 2
##’ 
##’ cbind2 (x, y) # works
##’ 
##’ y$y[3] <- 5
##’ try (cbind2 (x, y)) # error
##’ 
##’ 
bind <- function (direction = stop ("direction ('c' or 'r') required"),
                  ..., short = "bind", user = NULL, date = NULL){
  dots <- list (...)

  if ((length (dots) == 1) & is.list (dots [[1]]))
    dots <- dots[[1]]

  if (length (dots) == 0)
    NULL
  else if (length (dots) == 1){
    validObject (dots[[1]])
    dots[[1]]
  } else {                              # binding is actually needed.
    lapply (dots, chk.hy)
    lapply (dots, validObject)
    
    logs <- list()

    for (i in seq_along (dots) [-1]){
      dots[[1]] <- switch (direction,
                           c = cbind2 (dots[[1]], dots[[i]]),
                           r = rbind2 (dots[[1]], dots[[i]]),
                           stop ("direction must be either 'c' or 'r' for cbind",
                                 "and rbind, respectively.")
                           )

      dots [[1]] <- .logentry (dots [[1]], short = short,
                               long = list (direction = direction,
                                 .paste.row (dots [[i]])),
                               user = user, date = date)
      ## if (!is.null (short))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "short.description"] <- paste (short, dots[[1]]@log[nrow (dots[[1]]@log), "short.description"])
      ## if (!is.null (date))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "date"] <- date
      ## if (!is.null (user))
        ## dots[[1]]@log[nrow (dots[[1]]@log), "user"] <- user
    }
    
    dots [[1]]
  }
}

cbind.hyperSpec <- function (..., short = "cbind", deparse.level) bind ("c", ..., short = "cbind")
rbind.hyperSpec <- function (..., short = "rbind", deparse.level) bind ("r", ..., short = "rbind")

