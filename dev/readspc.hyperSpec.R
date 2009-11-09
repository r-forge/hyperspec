#####################################################################################################
###
### read.spc - Import Thermo Galactic's .spc file format into an hyperSpec Object
###
### C. Beleites 2009/11/08
###
#####################################################################################################


## Define constants ---------------------------------------------------------------------------------

## File Date Type Flags .............................................................................
## ORed to determine data type
## SPC File Format  Version (magic number) ..........................................................
.read.spc.FVERSN <- c (NEW.LSB = 75, NEW.MSB = 76, OLD = 77)
storage.mode (.read.spc.FVERSN) <- "raw"


## Instrumental Experiment Technique ................................................................
.read.spc.FEXPER <- c (SPCGEN  =  0,
                       SPCGC   =  1,
                       SPCCGM  =  2,
                       SPCHPLC =  3, 
                       SPCFTIR =  4,
                       SPCNIR  =  5,
                       SPCUV   =  6,
                       ## 7 is undefined
                       SPCXRY  =  8,
                       SPCMS   =  9,
                       SPCNMR  = 10,
                       SPCRMN  = 11,
                       SPCFLR  = 12,
                       SPCATM  = 13,
                       SPCDAD  = 14
                       )
storage.mode (.read.spc.FEXPER) <- "raw"

## 
## .read.spc.FEXP <- c(FLOAT = 128)
## storage.mode (.read.spc.FEXP) <- "raw"

## x-axis units .....................................................................................
.read.spc.FXTYPE <- c (expression (`/` (x, "a. u.")),                      #0
                       expression (`/` (tilde (nu), cm^-1)),
                       expression (`/` (lambda, (mu * m))),
                       expression (`/` (lambda, nm)),
                       expression (`/` (t, s)),
                       expression (`/` (t, min)),
                       expression (`/` (f, Hz)),
                       expression (`/` (f, kHz)),
                       expression (`/` (f, MHz)),
                       expression (`/` (frac (m, z), frac (u, e))),        
                       expression (`/` (delta, ppm)),                      # 10
                       expression (`/` (t, d)),
                       expression (`/` (t, a)),
                       expression (`/` (delta*tilde (nu), cm^-1)),
                       expression (`/` (E, eV)),
                       NA, # old version file uses label in gcatxt
                       'Diode No',
                       'Channel',
                       expression (`/` (x, degree)),
                       expression (`/` (T, degree*F)),
                       expression (`/` (T, degree*C)),                     # 20
                       expression (`/` (T, K)),
                       'Data Point',
                       expression (`/` (t, ms)),
                       expression (`/` (t, micro*s)),
                       expression (`/` (t, ns)),
                       expression (`/` (f, GHz)),
                       expression (`/` (lambda, cm)),
                       expression (`/` (lambda, m)),
                       expression (`/` (lambda, mm)),
                       expression (`/` (t, h))                             # 30
                       )

.read.spc.xlab <- function (x) {
  if (x <= length (.read.spc.FXTYPE) + 1)
    .read.spc.FXTYPE [x + 1]
  else
    NA
}

## y-axis units .....................................................................................
.read.spc.FYTYPE <- c (expression (`/` (I, "a. u.")),                      # 0
                       expression (`/` (I, "a. u.")),
                       'A',
                       expression (frac ((1 - R)^2, 2 * R)),
                       'Counts',
                       expression (`/` (U, V)),
                       expression (`/` (y, degree)),
                       expression (`/` (I, mA)),
                       expression (`/` (l, mm)),
                       expression (`/` (U, mV)),
                       expression (-log (R)),                              # 10
                       expression (`/` (y, '%')),
                       expression (`/` (I, 'a. u.')),        
                       expression (I / I[0]),                             
                       expression (`/` (E, J)),
                       NA, # old version file uses label in gcatxt
                       expression (`/` (G, dB)),
                       NA, # old version file uses label in gcatxt
                       NA, # old version file uses label in gcatxt
                       expression (`/` (T, degree*F)),
                       expression (`/` (T, degree*C)),                     # 20
                       'n',
                       'epsilon',
                       expression (Re (y)),
                       expression (Im (y)),
                       'T',
                       'R',
                       expression (`/` (I, 'a. u.')),
                       expression (`/` (I, 'a. u.'))
                       )
.read.spc.ylab <- function(x){
  if (x <= 26)
    .read.spc.FYTYPE [x + 1]
  else if (x %in% 128 : 131)
    .read.spc.FYTYPE [x - 101]
  else
    NA
}

## Date bit masks ...................................................................................
## .read.spc.FDATE.MIN <- c(as.raw (c(0, 0, 0, 63)))
## .read.spc.FDATE.H   <- c(as.raw (c(0, 0, 7, 192)))
## .read.spc.FDATE.D   <- c(as.raw (c(0, 0, 248, 0)))
## .read.spc.FDATE.M   <- c(as.raw (c(0, 15, 0, 0)))
## .read.spc.FDATE.Y   <- c(as.raw (c(255, 240, 0, 0)))


## file part reading functions ----------------------------------------------------------------------

## file header is always 512 bytes long

.read.spc.fileheader <- function (header) {
  if (header [2] != .read.spc.FVERSN ['NEW.LSB'])
    stop ("Wrong spc file format version.\n",
          "Only 'new' spc files (1996 file format) with LSB are supported.") 

  fdate  <- readBin (header [ 33 :  36], "integer", 1, 4, signed = FALSE)

  ftflgs <- readBin (header [1],         "integer", 1, 1, signed = FALSE)
  ftflgs <- as.logical (ftflgs %/% 2^(0 : 7) %% 2)
  names (ftflgs) <- c ('TSPREC', 'TCGRAM', 'TMULTI', 'TRANDM',
                       'TORDRD', 'TALABS', 'TXYXYS', 'TXVALS')
  
  list (ftflgs   = ftflgs,
        fexper   = readBin   (header [        3], "integer", 1, 1, signed = TRUE),
        fexp     = readBin   (header [        4], "integer", 1, 1, signed = TRUE),
        fnpts    = readBin   (header [  5 :   8], "integer", 1, 4, signed = FALSE),
        ffirst   = readBin   (header [  9 :  16], "double", 1),
        flast    = readBin   (header [ 17 :  24], "double", 1),
        fnsub    = readBin   (header [ 25 :  28], "integer", 1, 4, signed = FALSE),
        fxtype   = .read.spc.xlab (readBin(header [    29], "integer", 1, 1, signed = FALSE)),
        fytype   = .read.spc.ylab (readBin(header [    30], "integer", 1, 1, signed = FALSE)),
        fztype   = .read.spc.xlab (readBin(header [    31], "integer", 1, 1, signed = FALSE)),
        fpost    = readBin   (header [       32], "integer", 1, 1, signed = TRUE),
        fdate    = ISOdate (year  = fdate %/% 1048560,
          month = fdate %/% 65536 %%  16,
          day   = fdate %/% 2048 %% 32,
          hour  = fdate %/% 64 %% 32,
          min   = fdate %% 64),
        fres     = paste (rawToChar (header [ 37 :  45], multiple = TRUE), collapse = ""),
        fsource  = paste (rawToChar (header [ 46 :  54], multiple = TRUE), collapse = ""),
        fpeakpt  = readBin   (header [ 55 :  56], "integer", 1, 2, signed = FALSE),
        fspare   = readBin   (header [ 57 :  88], "numeric", 8, 4),
        fcmnt    = paste (rawToChar (header [ 89 : 218], multiple = TRUE), collapse = ""),
        fcatxt   = paste (rawToChar (header [219 : 248], multiple = TRUE), collapse = ""),
        flogoff  = readBin   (header [249 : 252], "integer", 1, 4, signed = FALSE),
        fmods    = readBin   (header [253 : 256], "integer", 1, 4, signed = FALSE),
        fprocs   = readBin   (header [      257], "integer", 1, 1, signed = TRUE),
        flevel   = readBin   (header [      258], "integer", 1, 1, signed = TRUE),
        fsampin  = readBin   (header [259 : 260], "integer", 1, 2, signed = FALSE),
        ffactor  = readBin   (header [261 : 264], "numeric", 1, 4),
        fmethod  = paste (rawToChar (header [265 : 312], multiple = TRUE), collapse = ""),
        fzinc    = readBin   (header [313 : 316], "numeric", 1, 4, signed = FALSE),
        fwplanes = readBin   (header [317 : 320], "integer", 1, 4, signed = FALSE),
        fwinc    = readBin   (header [321 : 324], "numeric", 1, 4),
        fwtype   = readBin   (header [      325], "integer", 1, 1, signed = TRUE)
        ## 187 bytes reserved
        )
}

## sub header is always 32 bytes long
.read.spc.subheader <- function (subheader) {
  list (subflgs   =          subheader [      1],
        subexp    = readBin (subheader [      2], "integer", 1, 1, signed = FALSE),
        subindx   = readBin (subheader [ 3 :  4], "integer", 1, 2, signed = FALSE),
        subtime   = readBin (subheader [ 5 :  8], "numeric", 1, 4),
        subnext   = readBin (subheader [ 9 : 12], "numeric", 1, 4),
        subnois   = readBin (subheader [13 : 16], "numeric", 1, 4),
        subnpts   = readBin (subheader [17 : 20], "integer", 1, 4, signed = FALSE),
        subscan   = readBin (subheader [21 : 24], "integer", 1, 4, signed = FALSE),
        subwlevel = readBin (subheader [25 : 28], "numeric", 1, 4)
        # 4 bytes reserved
        )
}

## subfile xy directory is always 12 bytes long
.read.spc.sfxyhdr <- function (ssfstc) {
  list (ssfposn = readBin (ssfstc [ 1 :  4], "integer", 1, 4 , signed = FALSE),
        ssfsize = readBin (ssfstc [ 5 :  8], "numeric", 1, 4),
        ssftime = readBin (ssfstc [ 9 : 12], "numeric", 1, 4)        
        )
}

## log header is always 64 bytes long
.read.spc.loghdr <- function (loghdr) {
  list (logsizd = readBin (loghdr [ 1 :  4], "integer", 1, 4 , signed = FALSE),
        logsizm = readBin (loghdr [ 5 :  8], "integer", 1, 4 , signed = FALSE),
        logtxto = readBin (loghdr [ 9 : 12], "integer", 1, 4 , signed = FALSE),
        logbins = readBin (loghdr [13 : 16], "integer", 1, 4 , signed = FALSE),
        logdsks = readBin (loghdr [17 : 20], "integer", 1, 4 , signed = FALSE),
        )
}

## header sizes  ------------------------------------------------------------------------------------
.read.spc.size <- c (hdr = 512, subhdr = 32, xydir = 12, loghdr = 64)


## some checks --------------------------------------------------------------------------------------

## file header ......................................................................................
##
## be sure to replace original file header by the return value: some settings are silently corrected!
##

.read.spc.fileheader.check <- function (header) {
  if (header$ftflgs ['TMULTI']){
    ## multiple spectra in file
    if (header$fnsub <= 1)
      warning ("spc file header specifies multiple spectra but only zero or one subfile.")
  } else {
    ## single spectrum file

    if (header$fnsub == 0)
      header$fnsub <- 1
    
    if (header$fnsub >  1) {
      warning ("spc file header specifies single spectrum file  but", header$fnsub,
               "subfiles (spectra).\nOnly first subfile will be read.")
      header$fnsub <- 1
    }

    if (header$ftflgs ['TRANDM'])
      warning ("spc file header: file type flag TRANDM does not make sense without TMULTI.")

    if (header$ftflgs ['TORDRD'])
      warning ("spc file header: file type flag TORDRD does not make sense without TMULTI.")

    if (header$ftflgs ['TRANDM'])
      warning ("spc file header: file type flag TRANDM does not make sense without TMULTI.")
  }

  if (header$ftflgs ['TXYXYS'] && ! header$ftflgs ['TXVALS'])
    warning ("spc file header: file type flag TXYXYS does not make sense without TXVALS.")
  
  header
}

## subfile header ...................................................................................
##
## be sure to replace original subheader by the return value: settings may be silently corrected!
##

.read.spc.subheader.check <- function (subheader, header, n) {
  if (subheader$subindx != n)
    stop ("subfile ", n + 1, ": wrong subfile index (", subheader$subindx, "instead of", n, ").")

  if (subheader$subexp == -128 && header$fexp != -128)
    warning ("subfile ", n + 1, " specifies data type float, but file header doesn't.",
             "\nData will be interpreted as float.")

  if (subheader$subnpts > 0 && ! header$ftflgs ['TXYXYS'])
    warning ('subfile ', n + 1, ": number of points in subfile should be 0 if file header flags",
             "do not specify TXYXYS.")

  if (subheader$subnpts == 0){
    if (header$ftflgs ['TXYXYS'])
      warning ('subfile ', n + 1, ': number of data points per spectrum not specified. ',
               'Using fnpts (', header$fnpts, ').')
    subheader$subnpts <- header$fnpts
  }

  if (! header$ftflgs ['TXYXYS'])
    if (header$fnpts != subheader$subnpts)
      stop ("header and subheader differ in number of points per spectrum, ",
            "but TXYXYS is not specified.")

  subheader
}


#####################################################################################################
read.spc <- function(filepath) {
  f <- readBin (filepath, "raw", file.info (filepath)$size, 1)

  header <- .read.spc.fileheader (f [seq_len (.read.spc.size ['hdr'])])
  header <- .read.spc.fileheader.check (header)
  fpos <- .read.spc.size ['hdr']

  
  wavelength <- NULL
  if (! header$ftflgs ['TXVALS'] && ! header$ftflgs ['TXYXYS'])
    ## files with common wavelength axis
    wavelength <- seq (header$ffirst, header$flast, length.out = header$fnpts)

  if (header$ftflgs ['TXVALS'] && ! header$ftflgs ['TXYXYS']) {
    wavelength <- readBin (f [fpos + seq_len (header$fnpts * 4)], "numeric", header$fnpts, 4)
    fpos <- fpos + header$fnpts * 4
  }

  ## subfile directory?
  ## the offset pointer is in header$fnpts if TXYXYS is set
  #if (header$ftflgs ['TXYXYS'])
  #  header$subfile.directory <- header$fnpts
  
  ## read subfiles
  for (s in seq_len (header$fnsub)) {
    subheader <- .read.spc.subheader (f [fpos + seq_len (.read.spc.size ['subhdr'])])
    subheader <- .read.spc.subheader.check (subheader, header, s - 1)
    fpos <-  fpos + .read.spc.size ['subhdr']

    if (header$ftflgs ['TXVALS'] && header$ftflgs ['TXYXYS']) {
      ## multi-spectra file with individual wavelength axes
      wavelength <- readBin (f [fpos + seq_len (header$subnpts * 4)], "numeric", header$fnpts, 4)
      fpos <-  fpos + header$fnpts * 4
    }

    ## read Y data
    if (subheader$subexp == -128) {
      ## 4 byte float
      y <- readBin (f [fpos + seq_len (subheader$subnpts * 4)], "numeric", header$subnpts, 4)
      fpos <-  fpos + header$fnpts * 4
    } else if (header$ftflgs ['TSPREC']) {
      ## 2 byte fixed point integer
      y <- 2 ^ (subheader$subexp - 16) * readBin (f [fpos + seq_len (subheader$subnpts * 2)],
                                                    "integer", header$subnpts, 2, signed = TRUE)
      fpos <-  fpos + header$fnpts * 2
    } else {
      ## 4 byte fixed point integer
      y <- 2 ^ (subheader$subexp - 32) * readBin (f [fpos + seq_len (subheader$subnpts * 4)],
                                                    "integer", subheader$subnpts, 4, signed = TRUE)
      fpos <-  fpos + header$fnpts * 4
    }
  }


 }
