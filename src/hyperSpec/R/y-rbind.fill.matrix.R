##' Bind matrices by row, and fill missing columns with NA
##'
##' The matrices are bound together using their column names or the column indices (in that order of
##' precedence.) Numeric columns may be converted to character beforehand, e.g. using format.
##' If a matrix doesn't have colnames, the column number is used.
##'
##' @aliases rbind.fill
##' @author C. Beleites
##' @seealso   \code{\link[base]{rbind}}, \code{\link[base]{cbind}}, \code{\link[plyr]{rbind.fill}}
##' @export
##' @callGraph
##' @keywords manip
##' @examples 
##'  A <- matrix (1:4, 2)
##'  B <- matrix (6:11, 2)
##'  A
##'  B
##'  rbind.fill.matrix (A, B)
##' 
##'  colnames (A) <- c (3, 1)
##'  A
##'  rbind.fill.matrix (A, B)
##' 
##' @param ... the matrices to rbind
##' @return a matrix
rbind.fill.matrix <- function (...){
  matrices <- list (...)
  
  ## check the arguments
  if (! all  (sapply (matrices, is.matrix)))
    stop ("Input ", which (! sapply (matrices, is.matrix)), "is no matrix.")
  
  ## if the matrices have column names, use them 
  lcols <- lapply (matrices, .cols)
  cols  <- unique (unlist (lcols))

  ## preallocate the new spectra matrix
  pos <- sapply (matrices, nrow)
  result <- matrix (NA, nrow = sum (pos), ncol = length (cols))

  ## make an index vector for the row positions
  pos <- c (0, cumsum (pos))

  ## fill in the new matrix 
  for (i in seq_along (matrices)){
    icols <- match (lcols[[i]], cols)
    result [(pos [i] + 1) : pos [i + 1], icols] <- matrices [[i]]
  }

  attr (result, "cols") <- cols
  colnames (result) <- cols
  
  result
}

.cols <- function (x){
  cln <- colnames (x)
  if (is.null (cln)) cln <- seq_len (ncol (x))

  cln
}
