\name{cov,hyperSpec,missing-method}
\alias{cov,hyperSpec,missing-method}
\alias{pooled.cov}
\title{Covariance matrices for hyperSpec objects}
\usage{
  \S4method{cov}{hyperSpec,missing}(x, y = NULL,
    use = "everything",
    method = c("pearson", "kendall", "spearman"))

  pooled.cov(x, groups, ...,
    regularize = 1e-05 * max(abs(COV)))
}
\arguments{
  \item{x}{hyperSpec object}

  \item{y}{not supported}

  \item{use,method}{handed to \code{\link[stats]{cov}}}

  \item{groups}{factor giving the groups}

  \item{...}{ignored}

  \item{regularize}{regularization of the covariance
  matrix. Set \code{0} to switch off

  \code{pooled.cov} calculates pooled covariance like e.g.
  in LDA.}

  \item{groups}{factor indicating the groups}

  \item{use}{an optional character string giving a method
  for computing covariances in the presence of missing
  values.  This must be (an abbreviation of) one of the
  strings \code{"everything"}, \code{"all.obs"},
  \code{"complete.obs"}, \code{"na.or.complete"}, or
  \code{"pairwise.complete.obs"}.}

  \item{method}{a character string indicating which
  correlation coefficient (or covariance) is to be
  computed.  One of \code{"pearson"} (default),
  \code{"kendall"}, or \code{"spearman"}, can be
  abbreviated.}
}
\value{
  covariance matrix of size \code{nwl (x)} x \code{nwl (x)}
}
\description{
  Covariance matrices for hyperSpec objects
}
\examples{
image (cov (chondro))
pcov <-  pooled.cov (chondro, chondro$clusters)
plot (pcov$means)
image (pcov$COV)
}
\author{
  C. Beleites
}
\seealso{
  \code{\link[stats]{cov}}
}

