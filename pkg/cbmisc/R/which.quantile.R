##' indices of quantiles
##'
##' Similar to \code{\link[base]{which.min}} and  \code{\link[base]{which.max}}. The indices correspond to \code{\link[stats]{quantile}} type 3.
##' @param x data
##' @param probs which quantiles to return
##' @param na.rm should \code{NA}s be removed before?
##' @return vector of indices corresponding to the quantiles
##' @author Claudia Beleites
##' @seealso \code{\link[stats]{quantile}}
##' @export 
##' @examples
##' x <-c (2.34, 5.83, NA, 9.34, 8.53, 6.42, NA, 8.07, NA, 0.77)
##' probs <- c (0, .23, .5, .6, 1)
##'
##' which.quantile (x, probs, na.rm = TRUE)
##'
##' x [which.quantile (x, probs, na.rm = TRUE)] == quantile (x, probs, na.rm = TRUE, type = 3)
which.quantile <- function (x, probs, na.rm = FALSE){
  if (! na.rm & any (is.na (x)))
    return (rep (NA_integer_, length (probs)))
  
  o <- order (x)
  n <- sum (! is.na (x))
  o <- o [seq_len (n)]

  nppm <- n * probs - 0.5
  j <- floor(nppm)
  h <- ifelse((nppm == j) & ((j%%2L) == 0L), 0, 1)
  j <- j + h
  
  j [j == 0] <- 1
  o[j]
}

