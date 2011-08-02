.by <- function (data, INDICES, FUN = NULL, ..., levelorder = TRUE){
  
  if (length (data$spc) == 0)
    stop ("empty spectra matrix.")
  
  FUN <- if (!is.null(FUN)) 
      match.fun(FUN)

  ## this function is internal. Checks should be done in the exported functions, so this one can be
  ## as fast as possible.
  ## INDICES should already be one factor
  ##  if (!is.list(INDICES))  
  ##      INDICES <- list(INDICES)
browser ()
  INDICES <- split (seq_len (nrow (data)), INDICES)

  FUNx <- function (index) FUN (data$spc [index, ], ...)
  
  spc <- lapply (INDICES, FUNx)

  spc <- tapply (data@data$spc, INDICES, FUN, ...)
  
  data
}

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title tapply for hyperSpec objects
##' @param data hyperSpec object
##' @param INDICES factor giving the groups
##' @param FUN aggregation function that works on a matrix
##' @param ... handed to \code{FUN}
##' @param simplify (doesn't apply to hyperSpec objects)
##' @param short,long,user,date handed to \code{\link{logentry}}
##' @param levelorder if TRUE, the rows of the result are in the order of the \code{levels
##' (INDICES)}. Otherwise,
##' @param drop should unused levels of \code{INDICES} be dropped?
##' @return hyperSpec object
##' @author Claudia Beleites
setMethod ("by", signature = signature (data = "hyperSpec", INDICES = "factor"),
           function (data, INDICES, FUN = NULL, ...,
                     simplify = stop ("not supported by hyperSpec objects"),
                     levelorder = TRUE, drop = TRUE, 
                     short = "tapply", long = NULL, user = NULL, date = NULL){
             validObject (data)

             if (drop) INDICES <- droplevels (INDICES)

             data <- .tapply (data@data, INDICES, FUN = FUN, ...)

             ## FUN could have changed the no. of columns of the spectra matrix
             validObject (data)

             .logentry(data, short = short, long = long, user = user, date = date)
           })
