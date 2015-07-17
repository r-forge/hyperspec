##' Import Raman Spectra/Maps from Witec Instrument via ASCII files
##'
##' \code{scan.txt.Witec} reads Witec ASCII files where the first column gives the wavelength
##' axes and the other columns the spectra. \code{scan.dat.Witec} reads Witec's ASCII exported data 
##' which comes in separate files with x and y data.
##' @title File Import Witec Raman
##' @param file filename or connection to ASCII file
##' @param points.per.line number of spectra in x direction of the map
##' @param lines.per.image number of spectra in y direction
##' @param type type of spectra: \code{single} for single spectra (including time series), \code{map} for imaging data.
##' @param nwl number of wavelengths, if \code{NULL}, \code{readLines} is used to determine
##' \code{nwl} automatically.
##' @param remove.zerospc is deprecated and will be removed soon. Use \code{\link{hy.setOptions} (file.remove.emptyspc = TRUE)} instead.
##' @param ...,quiet handed to \code{\link[base]{scan}}
##' @return a hyperSpec object
##' @author Claudia Beleites
##' @seealso \code{vignette ("fileio")} for more information on file import and
##'
##' \code{\link{options}} for details on options.
##' @export
scan.txt.Witec <- function (file = stop ("filename or connection needed"),
                            points.per.line = NULL,
                            lines.per.image = NULL,
                            nwl = 1024,
                            remove.zerospc = TRUE,
                            type = c ("single", "map"),
                            ...){
    
    ## Deprecated parameters
    if (!missing (remove.zerospc))
        warning ("Option 'remove.zerospc' is deprecated and will be removed soon. Use 'hy.setOptions (file.remove.emptyspc = TRUE)' instead.")
    
    if (is.null (nwl)){
        txt <- readLines (file)
        nwl <- length (txt)
        txt <- scan (text = txt, ...)
    } else {
        txt <- scan (file, ...)
    }
    
    ## check for valid data connection
    .check.con (file = file)    
    
    ## check for valid input
    type <- .check.valid (type, hdr = NULL, points.per.line, lines.per.image)
    
    dim (txt) <- c (length (txt) / nwl, nwl)
    
    ## fix: Witec/Andor may have final comma without values -> last line is NA only
    ## => delete last row if this happens for a map
    if (all (is.na (txt [nrow (txt), ])))
        txt <- txt [- nrow (txt), ]
    
    spc <- new ("hyperSpec", wavelength = txt [1, ], spc = txt [-1, ])
    
    if (!is.null (points.per.line))
        spc@data$x <- rep (seq_len (points.per.line), lines.per.image)
    
    if (!is.null (lines.per.image))
        spc@data$y <- rep (- seq_len (lines.per.image), each = points.per.line)
    
    ## consistent file import behaviour across import functions
    .fileio.optional (spc, file)
}


##' @rdname scan.txt.Witec
##' @param filex filename wavelength axis file
##' @param filey filename intensity file
##' @export
scan.dat.Witec <- function (filex = stop ("filename or connection needed"),
                            filey = sub ("-x", "-y", filex),
                            points.per.line = NULL,
                            lines.per.image = NULL,
                            type = c ("single", "map"),
                            ...,
                            quiet = hy.getOption ("debuglevel") < 1L){
    ## check valid data connection
    .check.con (filex = filex, filey = filey)
    
    ## check valid input
    type <- .check.valid (type = type, points.per.line = points.per.line, 
                          lines.per.image = lines.per.image)
    
    ## read data
    wl <- scan (file = filex, ..., quiet = quiet)
    spc <- scan (file = filey, ..., quiet = quiet)
    
    dim (spc) <- c (length (wl), length (spc) / length (wl))
    
    spc <- new ("hyperSpec", wavelength = wl, spc = t (spc))
    
    if (!is.null (points.per.line))
        spc@data$x <- rep (seq_len (points.per.line), lines.per.image)
    
    if (!is.null (lines.per.image))
        spc@data$y <- rep (- seq_len (lines.per.image), each = points.per.line)
    
    ## consistent file import behaviour across import functions
    .fileio.optional (spc, filey)
}

###checking file connection
.check.con <- function (headerfile, filex, filey, file){
    ## check for valid data connection
    if (!missing (headerfile) && !file.exists (headerfile))
        stop ("Header file not found!")
    
    if (!missing (filex) && !file.exists (filex))
        stop ("Wavelength axis file not found!")
    
    if (!missing (filey) && !file.exists (filey))
        stop ("Intensity file not found!")
    
    if (!missing (file) && !file.exists (file))
        stop ("Spectra file not found!")
}

###checking for valid input
.check.valid <- function (type, hdr, points.per.line, lines.per.image){
    ## check valid input
    type <- match.arg (type, c ("single", "map"))
    
    if (type == "single" && !missing (points.per.line) && !is.null (points.per.line) && points.per.line != 1)#TODO: better to prove for values > 1?
        warning ("points.per.line != 1 given for single spectrum")
    
    if (type == "single" && !missing (lines.per.image) && !is.null (lines.per.image) && lines.per.image != 1)#TODO: see above
        warning ("lines.per.image != 1 are defined for single spectrum")
    
    if (type == "single" && !missing (hdr) && !is.null (hdr) && hdr ["SizeY", ] != 1)
        warning ("header provides spatial information in y direction for single spectra")
    
    return (type)
}