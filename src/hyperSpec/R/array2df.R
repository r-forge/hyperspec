###-----------------------------------------------------------------------------
###
###  array2df -- "explodes" a mulitdimensional array into a long form matrix or
###              data.frame. Compare stack, unstack.
###
###



##’ array2df: Convert multidimensional array into matrix or data.frame
##’ The "wide-format" array is converted into a "long-format" \code{matrix} or
##’ \code{data.frame}.
##’ 
##’ If the resulting \code{data.frame} is too large to fit in memory, a
##’ \code{matrix} might help.
##’ 
##’ The main benefit of this function is that it uses matrices as long as
##’ possible. This can give large advantages in terms of memory consumption.
##’ 
##’ @param x \code{array}
##’ @param levels \code{list} with the levels for the dimensions of \code{x}.
##’ 
##’ If \code{levels[[i]]} is \code{NULL} no column is produced for this factor.
##’ 
##’ If \code{levels[[i]]} is \code{NA}, the result column is a numeric with
##’   range from \code{1} to \code{dim (x)[i]}
##’ 
##’ \code{names(levels)} yield the resulting column names.
##’ @param matrix If \code{TRUE}, a numeric \code{matrix} rather than a
##’   \code{data.frame} is returned.
##’ @param label.x Name for the column containing the \code{x} values.
##’ @return A \code{data.frame} or \code{matrix} with \code{prod (dim (x))}
##’   rows and \code{length (dim (x)) + 1} columns.
##’ @author C. Beleites
##’ @seealso \code{\link[utils]{stack}}
##’ @keywords array manip
##’ @examples
##’ 
##’ arr <- array (rnorm (24), 2:4)
##’ 
##’ array2df (arr)
##’ 
##’ array2df (arr, levels = list(NULL, x = NA, c = NULL), label.x = "value")
##’ 
##’ array2df (arr, levels = list(NULL, x = NA, c = letters [1:4]), label.x = "value")
##’ 
##’ summary (array2df (arr,
##’                    levels = list(NULL, x = NA, c = letters [1:4]),
##’                    label.x = "value"))
##’ 
##’ summary (array2df (arr,
##’                    levels = list(NULL, x = NA, c = letters [1:4]),
##’                    label.x = "value",
##’                    matrix = TRUE))
##’ 
##’ 
array2df <- function (x, levels = rep (NA, length (dims)),
                      matrix = FALSE,
                      label.x = deparse (substitute (x))){
  dims  <- dim (x)

  if (length (levels) != length (dims))
    stop ("Levels must have as many elements as x has dimensions.")
  
  cprod <- c(1, cumprod (dims))
  rprod <- c(rev (cumprod (rev (dims))), 1)[-1]
  idim  <- seq_along (dims) [! sapply (levels, is.null)]

  df <- matrix (x, nrow = length (x), ncol = length (idim) + 1)

  for (d in seq (along = idim))
    df [, d + 1] <-  rep (seq_len (dims [idim [d]]), each = cprod [idim [d]], times = rprod [idim [d]])

  if(!matrix){
    df <- as.data.frame (df)

    for (d in seq (along = idim)){
      if (! all (is.na(levels[[idim [d]]]))){
        df[, d + 1] <- factor (df[, d + 1], labels = levels [[idim [d]]])
      }
    }

  }
  colnames (df) <- c (label.x, names (levels)[idim])

  df
}
