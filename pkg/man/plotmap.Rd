\name{plotmap}
\alias{plotmap}
\alias{plotvoronoi}
\alias{levelplot,formula,hyperSpec-method}
\alias{levelplot,hyperSpec,missing-method}
\alias{map.identify}

\title{Plot a Map and Identify Spectra}
\description{
  \code{\link[lattice]{levelplot}} functions for hyperSpec objects.
  An image or map of a summary value of each spectrum is plotted. The
  spectra may be identified by mouse click.
}
\usage{
\S4method{levelplot}{formula,hyperSpec}(x, data, transform.factor =
TRUE, \dots)

\S4method{levelplot}{hyperSpec,missing}(x, data, \dots)

plotmap (object, model = spc ~ x * y,
         func = mean, func.args = list (), 
         \dots)

plotvoronoi (object, model = spc ~ x * y,
             use.tripack = TRUE, mix = TRUE,
             \dots)

map.identify (object,  model = spc ~ x * y, voronoi = FALSE, \dots)
}

\arguments{
  \item{object, data}{the \code{hyperSpec} object}

  \item{model, x}{formula specifying the columns of object that are to be
	 displayed by \code{\link[lattice]{levelplot}}}
  
  \item{transform.factor}{If the color-coded variable is a factor,
	 should \code{\link{trellis.factor.key}} be used to compute the color
	 coding and legend?}
  
  \item{func, func.args}{Before plotting, \code{plotmap} applies
	 function \code{func} with the arguments given in the list
	 \code{func.args} to each of the spectra. Thus a single summary value
	 is displayed for each of the spectra.}

  \item{use.tripack}{Whether package tripack should be used for
	 calculating the voronoi polygons. If \code{FALSE}, package deldir is
	 used instead. See details.}
  
  \item{mix}{For Voronoi plots using package tripack, I experienced
	 errors if the data was spatially ordered. Randomly rearrangig the rows
	 of the hyperSpec object circumvents this problem.}
  
  \item{voronoi}{Should the plot for identifying spectr by mouse click
	 be produced by \code{plotmap} (default) or \code{plotvoronoi}?}
  
  \item{\dots}{further arguments are passed down the call chain, and
	 finally to \code{\link[lattice]{levelplot}}
  }
}
\details{
  The \code{model} may contain the special column name
	 \code{.wavelength} to specify the wavelength axis.
  
  \code{plotmap}, \code{plotmap}, \code{map.identify}, and the
  \code{levelplot} methods internally use the same gateway function to
  \code{\link[lattice]{levelplot}}. Thus \code{transform.factor} can be
  used with all of them. Two special column names, \code{.rownames} and
  \code{.wavelength} may be used.

  \code{levelplot} plots the spectra matrix. 

  \code{plotvoronoi} calls \code{plotmap} with different default
  settings, namely the panel function defaults to
  \code{\link[latticeExtra]{panel.voronoi}}.
  \code{\link[latticeExtra]{panel.voronoi}} depends on either of the
  packages 'tripack' or 'deldir' being installed. For further
  information, please consult the help page of
  \code{\link[latticeExtra]{panel.voronoi}}.
  On the \code{\link{chondro}} data set, \code{plotmap} is roughly 5
  times faster than \code{plotvoronoi} using tripack, and ca. 15 times
  faster than \code{plotvoronoi} using deldir. Package tripack, however, is
  free only for non-commercial use.
  
  \code{map.identify} calls \code{plotmap} and \code{plotvoronoi},
  respectively and waits for (left) mouse clicks on points. Other mouse
  clicks end the input.
  
  Unlike \code{\link[lattice]{panel.identify}}, the indices returned by
  \code{map.identify} are in the same order as the points were
  clicked. Also, multiple clicks on the same point are returned as
  multiple entries with the same index.
}
\value{
  \code{map.identify} returns a vector of row indices into \code{object}
  of the clicked points.

  The other functions return a lattice object.
}
\author{C. Beleites}
\seealso{
  \code{vignette (mapplot)}, \code{vignette (introduction)}

  \code{\link{plot}}

  \code{\link[lattice]{levelplot}}
  \code{\link[latticeExtra]{panel.voronoi}} 
}
\examples{
levelplot (chondro)                     # throws warning: only first wl is used
levelplot (spc ~ y * x, chondro [,,1003]) # properly rotated
levelplot (chondro [,,1003], aspect = "iso")
plotmap (chondro [,,1003])

# plot spectra matrix
levelplot (spc ~ .wavelength * t, laser, contour = TRUE, col = "#00000080")
plot (flu, "mat")

# applying a function before plotting
plotmap (chondro, func = max, col.regions = gray (seq (0, 1, 0.05)))

plotmap (chondro, clusters ~ x * y)
plotmap (chondro, clusters ~ x * y, transform.factor = FALSE)
plotmap (chondro, clusters ~ x * y, col.regions = gray (seq (0, 1, 0.05))) 

# Voronoi plots
smpl <- sample (chondro, 300)
plotmap (smpl, clusters ~ x * y)
if (require (tripack)) 
    plotvoronoi (smpl, clusters ~ x * y,
                 border = "#00000020", col = "#00000020")
if (require (deldir)) 
    plotvoronoi (smpl, clusters ~ x * y,
                 border = "#00000020", col = "#00000020",
                 use.tripack = FALSE)
}
\keyword{hplot}

