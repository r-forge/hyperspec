\name{stacked.offsets}
\Rdversion{1.1}
\alias{stacked.offsets}
\title{y Offsets for Stacked Plots}
\description{
  Calculate approriate \code{yoffset} values for stacking in
  \code{\link[hyperSpec]{plotspc}}.	
}
\usage{
stacked.offsets(x, stacked = TRUE,
                min.zero = FALSE, add.factor = 0.05, add.sum = 0,
                .spc = NULL)
}
\arguments{
  \item{x}{a \code{hyperSpec} object}
  \item{stacked}{\code{TRUE} to stack single spectra. 
	 A numeric or factor is interpreted as giving the grouping, 
	 a character is interpreted as the name 
	 of the extra data column of \code{x} that holds the grouping.}
  \item{min.zero}{if \code{TRUE}, the lesser of zero and the minimum
	 intensity of the spectrum is used as minimum.}
  \item{add.factor, add.sum}{proportion and absolute amount of space
	 that should be added.}
  \item{.spc}{for internal use. If given, the ranges are evaluated on
	 \code{.spc}. However, this may change in future.}
}
\details{
Usually, the \code{stacked} argument of \code{\link[hyperSpec]{plotspc}} will do fine, but if you 
need fine control over the stacking, you may calculate the y offsets yourself. 
}
\value{
a list containing
\item{offsets}{numeric with the yoffset for each group in \code{stacked}}
\item{groups}{numeric with the group number for each spectrum}
\item{levels}{if \code{stacked} is a factor, the levels of the groups}
}
\author{C. Beleites}
\seealso{
\code{\link[hyperSpec]{plotspc}}
}
\examples{
mean.pm.sd <- aggregate (chondro, chondro$clusters, mean_pm_sd)

offset <- stacked.offsets (mean.pm.sd, ".aggregate")
plot (mean.pm.sd, fill.col = matlab.palette (3), fill = ".aggregate",
      stacked = ".aggregate")
 	
plot (aggregate (chondro, chondro$clusters, mean), yoffset = offset$offsets,
      lines.args = list (lty = 2, lwd = 2), add = TRUE)

barb <- do.call (collapse, barbituates [1:3])
plot (barb, lines.args = list (type = "h"), stacked = TRUE,
      stacked.args = list (add.factor = .2))

}
