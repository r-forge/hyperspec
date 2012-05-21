
.rmvnorm <- function (n, mean, sigma = cov (mean), ...) {
  .group <- rep.int (seq_along (n), n)

  data <- mean [.group]
 
  for (i in seq_along (n))
    data@data$spc [.group == i,] <- mvtnorm::rmvnorm (n [i], mean@data$spc [i, ], sigma)

  data$.group <- .group

  .logentry (data, ...)
}


rnd <- rmvnorm (rep (1e2, 3), x = chondro, groups = chondro$clusters)

tmp <- chondro [! is.na (chondro$clusters)]
lda <- lda (x = tmp [[]], grouping = tmp$clusters)

p <- predict (lda, rnd [[]])
plot (p$x, col = c("#00008040", "#FF000040", "#00800040") [p$class], pch = 3)

pc <- predict (lda)
points (pc$x, col = pc$class, pch = 20, cex = 0.5)




  setGeneric ("rmvnorm",
            def = function (n,
              mean = rep(0, nrow(sigma)),
              sigma = diag(length(mean)), 
              method = c("eigen", "svd", "chol"), ...) {
              mvtnorm::rmvnorm (n, mean = mean, sigma = sigma, method = method)
            })



setMethod ("rmvnorm", signature (n = "numeric", mean = "hyperSpec", sigma = "matrix"), .rmvnorm)

#setMethod ("rmvnorm", signature (n = "numeric", mean = "missing", sigma = "missing",
#                                 x = "hyperSpec", groups = "factor"),
rmvnorm <- function (n, x, groups, ..., reg = 1e-5 * max (abs (sigma))) {

             if (any (is.na (groups))) {
                 warning ("Removing NAs from groups.")
                 x <- x [! is.na (groups)]
                 groups <- groups [!is.na (groups)]
               }
             
             mean <- aggregate (x, groups, "mean")
             sigma <- cov (x [[]] - mean [[as.numeric (groups)]])

             ## regularization
             sigma <- sigma + diag (reg, nrow (sigma))
             
             .rmvnorm (n, mean = mean, sigma = sigma, ...)
           }
           
#           )    

