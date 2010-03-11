\name{rbind.fill.hyperSpec}
\alias{rbind.fill.hyperSpec}
\title{collapse several hyperSpec objects into one object...}
\usage{rbind.fill.hyperSpec(..., orderwl=TRUE, short="collapse", user, date)
rbind.fill.hyperSpec(..., orderwl=TRUE, short="collapse", user, date)
}
\description{collapse several hyperSpec objects into one object}
\details{The spectra from all objects will be put into one object.
The resulting object has all wavelengths that occur in the input objects.
Data points corresponding to wavelengths not in the original spectrum will be set to NA.
Extra data is combined in the same manner.\code{rbind.fill.hyperSpec}: The spectra from all objects will be put into one object.
The resulting object has all wavelengths that occur in the input objects.
Data points corresponding to wavelengths not in the original spectrum will be set to NA.
Extra data is combined in the same manner.


}
\author{C. Beleites}
\value{a hyperSpec object\code{rbind.fill.hyperSpec}: a hyperSpec object


}
\arguments{\item{...}{hyperSpec objects to be collapsed into one object. Instead of giving several arguments, a list with all objects to be collapsed may be given.}
\item{orderwl}{}
\item{short,user,date}{handed to logentry}
}
\examples{A <- new ("hyperSpec", spc = c(0, 1, .2), wavelength = c (1, 3, 7))
B <- new ("hyperSpec", spc = runif (4), wavelength = c (1, 2, 4, 5))
plotspc (A)
plotspc (B, add = TRUE, col = "darkgray")
plotspc (collapse (A, B), add = TRUE, col = c("red", "blue"),
lines.args = list (type = "p", pch = 20))

A$.
B$.
collapse (A,B)$.A <- new ("hyperSpec", spc = c(0, 1, .2), wavelength = c (1, 3, 7))
B <- new ("hyperSpec", spc = runif (4), wavelength = c (1, 2, 4, 5))
plotspc (A)
plotspc (B, add = TRUE, col = "darkgray")
plotspc (collapse (A, B), add = TRUE, col = c("red", "blue"),
lines.args = list (type = "p", pch = 20))

A$.
B$.
collapse (A,B)$.}

\alias{rbind.fill.hyperSpec}

