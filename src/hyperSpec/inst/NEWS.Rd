\name{hyperSpec NEWS}
\title{hyperSpec News}
\encoding{UTF-8}

\section{CHANGES IN hyperSpec 0.99}{
\subsection{User-visible changes}{
\itemize{
  \item new file import functions, (see vignette fileio)
  \itemize{
	 \item scan.txt.Witec: Witec ASCII files

	 \item read.jdx.Shimadzu: JCAMP-DX as produced by Shimadzu's GCxGC-MS
	 library (experimental)

	 \item read.txt.Shimadzu: ASCII files exported by Shimadzu Chrome
	 Solution (v. 2.73)
  }

  \item hyperSpec gains a subset method

  \item bugfix for stacked plots: y axis labels were in wrong order in
  some cases

  \item recycling in initialize: columns may (again) be initialized with
  a single value

  \item levelplot (incl. plotmap) now uses `useRaster`. This allows
 better control for falling back to `panel.levelplot`.
}
}
\subsection{Minor changes}{
  \itemize{
	 
	 \item levelplot uses .wavelength factors for subsetting, grouping,
	 conditioning: thus the strips will display the wavelength value.

	 \item extraction ([]) now warns if unused arguments are given

}
}
\subsection{Experimental new features}{
  \itemize{

	 \item mixed colour map plots: qplotmixmap (experimental)

}
}
\subsection{Bug fixes}{
  \itemize{

	 \item bugfix for unsigned long int in .spc files 

	 \item stacked spectra plots label positions
}
}
\subsection{Internal changes}{
  \itemize{

\item Documentation format is now roxygen2
}
}
}
 

