\name{rmvnorm}
\alias{rmvnorm}
\alias{rmvnorm,numeric,hyperSpec,array-method}
\alias{rmvnorm,numeric,hyperSpec,matrix-method}
\alias{rmvnorm,numeric,matrix,array-method}
\alias{rmvnorm,numeric,matrix,matrix-method}
\title{Multivariate normal random numbers}
\usage{
  \S4method{rmvnorm}{numeric,hyperSpec,matrix}(n, mean,
    sigma)

  \S4method{rmvnorm}{numeric,hyperSpec,array}(n, mean,
    sigma)

  \S4method{rmvnorm}{numeric,matrix,matrix}(n, mean,
    sigma = cov(mean))

  \S4method{rmvnorm}{numeric,matrix,array}(n, mean,
    sigma = cov(mean))
}
\arguments{
  \item{n}{vector giving the numer of cases to generate for
  each group}

  \item{mean}{matrix with mean cases in rows}

  \item{sigma}{common covariance matrix or array
  (\code{ncol (mean)} x \code{ncol (mean)} x \code{nrow
  (mean)}) with individual covariance matrices for the
  groups.}
}
\description{
  Interface functions to use \code{\link[mvtnorm]{rmvnorm}}
  for \code{\link[hyperSpec]{hyperSpec-class}} objects.
}
\details{
  The \code{mvtnorm} method for hyperSpec objects supports
  producing multivariate normal data for groups with
  different mean but common covariance matrix, see the
  examples.
}
\examples{
## multiple groups, common covariance matrix



pcov <- pooled.cov (chondro, chondro$clusters)
rnd <- rmvnorm (rep (1e2, 3), mean = pcov$mean, sigma = pcov$COV)

require ("MASS")
tmp <- chondro [! is.na (chondro$clusters)]
lda <- lda (x = tmp [[]], grouping = tmp$clusters)

p <- predict (lda, rnd [[]])

colors <- c("#00008040", "#FF000040", "#00800040")

plot (p$x, col = colors [p$class], pch = 3)

pc <- predict (lda)
points (pc$x, col = colors [pc$class], pch = 20, cex = 0.5)
}
\seealso{
  \code{\link[mvtnorm]{rmvnorm}}

  \code{\link[hyperSpec]{cov}} and
  \code{\link[hyperSpec]{pooled.cov}} about calculating
  covariance of hyperSpec objects.
}

