\name{scan.txt.Renishaw}
\alias{scan.txt.Renishaw}
\title{import Raman measurements from Renishaw ASCII-files	}
\usage{scan.txt.Renishaw(file=stop("filename is required"), data="xyspc",
    nlines=0, nspc, ...)}
\description{import Raman measurements from Renishaw .txt file}
\details{Renishaw .wxd files are converted to .txt ASCII files by their batch converter.
They come in a "long" format with columns (y x | time | z) wavelength intensity. 
The first columns depend on the data type.}
\value{the \code{hyperSpec} object}
\seealso{\code{\link{read.txt.long}}, \code{\link{read.txt.wide}}, \code{\link[base]{scan}}}
\author{cb}
\arguments{\item{file}{file name}
\item{data}{type of file, one of "spc", "xyspc", "zspc", "depth", "ts", see details.}
\item{nlines}{number of lines to read in each chunk, if 0 or less read whole file at once}
\item{nspc}{number of spectra in the file}
\item{...}{passed to logentry}}
