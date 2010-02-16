scan.txt.PerkinElmer <- function (files = "*.txt", skip = 54, ...) {
  ## find all files
  files <- Sys.glob (files)

  ## read the first file
  buffer <- matrix (scan (files [1], skip = skip, ...), ncol = 2, byrow = TRUE)

  ## first file gives the wavelength vector
  wavelength <- buffer [, 1]

  ## preallocate the spectra matrix: one row per file x as many columns as the 
  ## first file has
  spc <- matrix (ncol = nrow (buffer), nrow = length (files))

  ## the first file's data goes into the first row
  spc [1, ] <-  buffer [, 2]

  ## now read the remaining files
  for (f in seq (along = files)[-1]) {
    buffer <- matrix (scan (files [f], skip = skip, ...), 
                      ncol = 2, byrow = TRUE)

    ## check whether they have the same wavelength axis
    if (! all.equal (buffer [, 1], wavelength))
      stop (paste(files [f], "has different wavelength axis."))
    
    spc [f, ] <- buffer[, 2]
  }

  ## finally: make the hyperSpec object
  new ("hyperSpec", wavelength = wavelength, spc = spc,
       label = list (.wavelength = expression (lambda[fl] / nm),
         spc = "I / a.u."))
}
