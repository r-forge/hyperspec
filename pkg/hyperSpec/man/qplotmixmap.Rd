\name{qplotmixmap}
\alias{qplotmixmap}
\title{qplotmap with colour mixing for multivariate overlay}
\usage{qplotmixmap(object, ...)
}
\description{map plot with colour overlay.}
\value{invisible list with ggplot2 objects map and legend}
\seealso{\code{\link[hyperSpec]{qmixtile}}}
\author{Claudia Beleites}
\arguments{\item{object}{hyperSpec object}
\item{...}{handed over to \code{\link[hyperSpec]{qmixtile}} and \code{\link[hyperSpec]{qmixlegend}}}
}
\examples{
chondro <- chondro - spc.fit.poly.below (chondro)
chondro <- sweep (chondro, 1, apply (chondro, 1, mean), "/")
chondro <- sweep (chondro, 2, apply (chondro, 2, quantile, 0.05), "-")

qplotmixmap (chondro [,,c (940, 1002, 1440)],
purecol = c (colg = "red", Phe = "green", Lipid = "blue"))
}

