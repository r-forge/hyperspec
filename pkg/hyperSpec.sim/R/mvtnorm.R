##' @noRd
setGeneric ("rmvnorm", package = "mvtnorm")

.rmvnorm <- function (n, mean, sigma = cov (mean)) {
  .group <- rep.int (seq_along (n), n)

  data <- mean [.group]

  ## make indices so that pooled or individual covariance matrices can be used.
  if (length (dim (sigma)) == 3L)
    isigma <- seq_len (dim (sigma) [3])
  else {
    isigma <- rep (1L, nrow (mean))
    dim (sigma) <- c (dim (sigma), 1L)
  }
    
  for (i in seq_along (n))
    data@data$spc [.group == i,] <- mvtnorm::rmvnorm (n [i], mean@data$spc [i, ], sigma [,, isigma [i]])

  data$.group <- .group

  data
}

##' Multivariate normal random numbers
##'
##' Interface functions to use \code{\link[mvtnorm]{rmvnorm}} for
##' \code{\link[hyperSpec]{hyperSpec-class}} objects.
##'
##' The \code{mvtnorm} method for hyperSpec objects supports producing multivariate normal data for
##' groups with different mean but common covariance matrix, see the examples.
##'
##' @param n vector giving the numer of cases to generate for each group
##' @param mean matrix with mean cases in rows
##' @param sigma common covariance matrix or array (\code{ncol (mean)} x \code{ncol (mean)} x \code{nrow (mean)}) with individual covariance matrices for the groups.
##' @export
##' @seealso \code{\link[mvtnorm]{rmvnorm}}
##'
##' \code{\link[hyperSpec]{cov}} and \code{\link[hyperSpec]{pooled.cov}} about calculating  covariance of hyperSpec objects.
##' @rdname rmvnorm
##' @examples
##' ## multiple groups, common covariance matrix
##'
##' 
##' 
##' pcov <- pooled.cov (chondro, chondro$clusters)
##' rnd <- rmvnorm (rep (1e2, 3), mean = pcov$mean, sigma = pcov$COV)
##' 
##' require ("MASS")
##' tmp <- chondro [! is.na (chondro$clusters)]
##' lda <- lda (x = tmp [[]], grouping = tmp$clusters)
##' 
##' p <- predict (lda, rnd [[]])
##' 
##' colors <- c("#00008040", "#FF000040", "#00800040")
##' 
##' plot (p$x, col = colors [p$class], pch = 3)
##' 
##' pc <- predict (lda)
##' points (pc$x, col = colors [pc$class], pch = 20, cex = 0.5)

setMethod ("rmvnorm", signature (n = "numeric", mean = "hyperSpec", sigma = "matrix"),
           .rmvnorm)

##' @rdname rmvnorm
##' @export
setMethod ("rmvnorm", signature (n = "numeric", mean = "hyperSpec", sigma = "array"),
           .rmvnorm)


## produces matrices instead of hyperSpec objects. 
## mapply (rmvnorm, n = 1:3, mean = pcov$mean, MoreArgs= list (sigma = pcov$COV), SIMPLIFY = FALSE))
