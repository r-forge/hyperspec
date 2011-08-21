\name{spikes.interactive}
\alias{spikes.interactive}
\title{Interactive spike filtering...}
\usage{spikes.interactive(x, spikiness, npts=10, nspc=1, zoomfactor=4, ...)
}
\description{Interactive spike filtering}
\details{These functions calculate the suspiciousness of data points by a
\code{c (-1, 2, -1)} filter. \code{spikefilter} applies this filter along
the spectrtal (wavelength) direction, \code{spikefilter2d} in addition also
to neighbour spectra. \code{make.suspicions} prepares the spikes for use
in this GUI.

The recognition of spikes may be greatly improved by preprocesing the
spectra specially for this task, see the demo.}
\value{indices of the marked spectra}
\arguments{\item{spc}{hyperSpec object holding the spectra.}
\item{spikiness}{matrix from make.suspicions}
\item{npts}{initial wavelength axis zoom: the suspicious point +/- \code{npts}
points are displayed}
\item{nspc}{initial number of neighbour spectra: the suspicious spectrum +/- 
\code{nspc} spectra are displayed}
\item{save.tmp}{ignored at the moment}
\item{use.tmp}{ignored at the moment}
\item{ispikes}{ignored at the moment}
\item{iispikes}{ignored at the moment}
}

