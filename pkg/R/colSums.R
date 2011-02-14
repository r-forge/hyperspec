#  From base/R/colSums.R

.colSums <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])

    dn [1L : dims] <- 1L
    
    z <- if(is.complex(x))
        .Internal(colSums(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colSums(Im(x), n, prod(dn), na.rm))
    else .Internal(colSums(x, n, prod(dn), na.rm))

    if (drop){
      if(length(dn) > dims + 1L) {
        dim(z) <- dn[-(1L:dims)]   
        dimnames(z) <- dimnames(x)[-(1L:dims)]
      } else names(z) <- dimnames(x)[[dims+1]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}

.colMeans <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    n <- prod(dn[1L:dims])
    dn [1L : dims] <- 1L
    z <- if(is.complex(x))
        .Internal(colMeans(Re(x), n, prod(dn), na.rm)) +
            1i * .Internal(colMeans(Im(x), n, prod(dn), na.rm))
    else .Internal(colMeans(x, n, prod(dn), na.rm))

    if (drop){
      if(length(dn) > dims + 1L) {
        dim(z) <- dn[-(1L:dims)]   
        dimnames(z) <- dimnames(x)[-(1L:dims)]
      } else names(z) <- dimnames(x)[[dims+1]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}

.rowSums <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])

    dn [(dims + 1L) : length (dn)] <- 1L

    z <- if(is.complex(x))
        .Internal(rowSums(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowSums(Im(x), prod(dn), p, na.rm))
    else .Internal(rowSums(x, prod(dn), p, na.rm))

    if (drop){
      if(dims > 1L) {
        dim(z) <- dn [1L:dims]
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}

.rowMeans <- function(x, na.rm = FALSE, dims = 1L, drop = TRUE)
{
    if(is.data.frame(x)) x <- as.matrix(x)
    if(!is.array(x) || length(dn <- dim(x)) < 2L)
        stop("'x' must be an array of at least two dimensions")
    if(dims < 1L || dims > length(dn) - 1L)
        stop("invalid 'dims'")
    p <- prod(dn[-(1L:dims)])

    dn [(dims + 1L) : length (dn)] <- 1L

    z <- if(is.complex(x))
        .Internal(rowMeans(Re(x), prod(dn), p, na.rm)) +
            1i * .Internal(rowMeans(Im(x), prod(dn), p, na.rm))
    else .Internal(rowMeans(x, prod(dn), p, na.rm))

    if (drop){
      if(dims > 1L) {
        dim(z) <- dn [1L:dims]
        dimnames(z) <- dimnames(x)[1L:dims]
    } else  names(z) <- dimnames(x)[[1L]]
    } else {
      dim(z) <- dn
      dimnames(z) <- dimnames(x)
    }
    z
}

test (.rowSums) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::rowSums (a, dims = d)
    drop <- rowSums (a, dims = d, drop = TRUE)
    nodrop <- rowSums (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [1 : d], dd, sprintf ("result dimensions, d = %i", d))
  }
}

test (.rowMeans) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::rowMeans (a, dims = d)
    drop <- rowMeans (a, dims = d, drop = TRUE)
    nodrop <- rowMeans (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [1 : d], dd, sprintf ("result dimensions, d = %i", d))
  }
}

test (.colSums) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::colSums (a, dims = d)
    drop <- colSums (a, dims = d, drop = TRUE)
    nodrop <- colSums (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [-(1L : d)], dd, sprintf ("result dimensions, d = %i", d))
  }
}

test (.colMeans) <- function (){
  a <- array (1:24, 4:2)
  for (d in 1 : 2){
    default <- base::colMeans (a, dims = d)
    drop <- colMeans (a, dims = d, drop = TRUE)
    nodrop <- colMeans (a, dims = d, drop = FALSE)

    checkEquals (default, drop, sprintf ("base version ./. drop = TRUE, dim = %i", d))
    checkEquals (c (default), c (nodrop), sprintf ("drop = TRUE ./. FALSE, dim = %i", d))

    dd <- dim (default)
    if (is.null (dd)) dd <- length (default)
    checkEquals (dim (nodrop) [-(1L : d)], dd, sprintf ("result dimensions, d = %i", d))
  }
}

setMethod ("colSums", signature = "matrix", .colSums)
setMethod ("colSums", signature = "array", .colSums)
setMethod ("colMeans", signature = "matrix", .colMeans)
setMethod ("colMeans", signature = "array", .colMeans)
setMethod ("rowSums", signature = "matrix", .rowSums)
setMethod ("rowSums", signature = "array", .rowSums)
setMethod ("rowMeans", signature = "matrix", .rowMeans)
setMethod ("rowMeans", signature = "array", .rowMeans)

