\name{spikefilter}
\alias{spikefilter2d}
\title{Calculate spike suspiciousness...}
\usage{spikefilter2d(spcmatrix)
spikefilter(spcmatrix)
}
\description{Calculate spike suspiciousness}
\details{\code{spikefilter2d}: These functions calculate the suspiciousness of data points by a
\code{c (-1, 2, -1)} filter. \code{spikefilter} applies this filter along
the spectrtal (wavelength) direction, \code{spikefilter2d} in addition also
to neighbour spectra.

The recognition of spikes may be greatly improved by preprocesing the
spectra specially for this task, see the demo.

}
\value{\code{spikefilter2d}: matrix of the same size as \code{spcmatrix}

}
\arguments{\item{spcmatrix}{spectra matrix}
}
\alias{spikefilter}

