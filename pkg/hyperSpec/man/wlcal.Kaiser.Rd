\name{wlcal.Kaiser}
\alias{wlcal.Kaiser}
\title{Wavelength and Wavenumber calibration for Kaiser Raman}
\usage{
  wlcal.Kaiser(spc, wcl, laser,
    range = c("both", "low", "high"))
}
\arguments{
  \item{spc}{hyperSpec object with wavelength in CCD
  pixels}

  \item{wcl}{connection to Kaiser wavelength calibration
  (.wcl) file, see details.}

  \item{laser}{exact laser wavelength or connection to a
  Kaiser Hologram .spc file}

  \item{range}{which spectral ranges does the hyperSpec
  object contain?}
}
\value{
  wavelength-calibrated hyperSpec object
}
\description{
  Wavelength and Wavenumber calibration for Kaiser
}
\details{
  This function performs two calibration steps for the
  spectral abscissa:

  \bold{wavelength} calibration converts the CCD's pixel
  number into wavelengths (in nm). This uses the
  calibration settings read from sections "Chan1 Plex1
  Parameters" and "Chan1 Plex2 Parameters" of the .wcl
  file. If \code{wcl} is not given, this step is skipped
  (i.e. the wavelength is supposed to hold already a proper
  wavelength).

  \bold{wavenumber} calibration converts the wavelengths
  into Raman shift relative wavenumbers. This needs the
  exact wavelength of the excitation laser (in nm).
  Alternatively, an .spc file written by Kaiser Hologram
  can be given. These files record the the laser wavelength
  in log entry "Laser_Wavelength". If \code{laser} is
  missing, this step is skipped and wavelengths are
  returned.
}
\author{
  Claudia Beleites
}
\seealso{
  \code{\link{read.spc.Kaiser}} for importing Kaiser .spc
  files

  \code{\link{wl2relwn}} for wavelength to Raman shift
  (relative wavenumber) conversion.
}

