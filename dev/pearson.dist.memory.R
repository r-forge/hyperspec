# TODO: Add comment
# 
# Author: Claudia Beleites
#
# Created: 2:43:16 PM Feb 26, 2009
###############################################################################

pearson.dist.memory <- function (x, mod = 1000){
dummy <- apply (x, 1, sum)
x <- sweep (x, 1, dummy / ncol (x))

#cov <- (x %*% t(x)) / (ncol (x) - 1)

dummy <- x^2 
dummy <- sqrt (apply (dummy, 1, sum) / (ncol (x) - 1))
x <- sweep (x, 1, dummy, "/")

#cor <- (x %*% t(x)) / (ncol (x) - 1)
#cor [1, 2] <- sum (x [1,] * x[2,]) / (ncol (x) - 1) 

n <- nrow (x)
dist <- numeric (n * (n - 1) / 2)	
attr(dist, "Size") <- n
attr(dist, "Labels") <- dimnames(x)[[1]]
attr(dist, "diag") <- FALSE
attr(dist, "Upper") <- FALSE
attr(dist, "method") <- "pearson"
attr(dist, "call") <- match.call()
class(dist) <- "dist"

x <- t (x)
pos <- 0
for (i in seq_len (n - 1)){
	j <-  seq (1, n - i)
	
	dummy <- x [,j + i, drop = FALSE] * x [,i]
	#tst <- sweep (x[,j], 1, x[,i], "*")
	#range (dummy)
	#range (dummy - tst)
	dist  [pos + j] <- colSums (dummy)
	pos <- pos + n - i
	if (i %% mod == 0) cat (".")
}
dist <- dist / (nrow (x) - 1)
dist <- (1 - dist) / 2
cat ("\n")
dist
}

