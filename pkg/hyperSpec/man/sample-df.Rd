\name{sample-df}
\alias{sample,data.frame-method}
\alias{sample}
\title{Random Samples and Permutations: data.frame}
\usage{\S4method{sample}{data.frame}(x, size=nrow(x), replace=FALSE, prob=NULL, drop=FALSE)

\S4method{sample}{matrix}(x, size=nrow(x), replace=FALSE, prob=NULL, drop=FALSE)

}
\description{Random row-wise sample/permutation of data.frames}
\details{\code{sample,data.frame-method}: Draws a random row sample of the data.frame

}
\value{\code{sample,data.frame-method}: a data.frame or matrix with \code{size} rows

}
\seealso{\code{\link[base]{sample}}}
\author{Claudia Beleites}
\arguments{\item{x}{the data.frame or matrix to sample from}
\item{size,replace,prob}{handed to \code{\link[base]{sample.int}}}
\item{drop}{see \code{\link[base]{drop}}: by default, do not drop dimensions of the result}
}
\examples{
sample (cars, 2)

sample (matrix (1:24, 6), 2)
}
\alias{sample,matrix-method}
\alias{sample}

