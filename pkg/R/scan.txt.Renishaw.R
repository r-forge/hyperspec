###-----------------------------------------------------------------------------
###
### scan.txt.Renishaw - import Renishaw Raman ASCII files
###
### In general this is a long ASCII format. But this function offers chunk-wise
### reading to save memory.
###

scan.txt.Renishaw <- function (file = stop ("filename is required"),
                               data = "xyspc", nlines = 0, nspc = NULL, 
                               short = "scan.txt.Renishaw", user = NULL,
                               date = NULL){
  cols <- switch (data,
                  spc = NULL,
                  xyspc = list (y = expression ("/" (y, mu * m)), 
                    x = expression ("/" (x, mu * m))), 
                  zspc = ,
                  depth = list (z = expression ("/" (z, mu * m))),
                  ts = 	list (t = "t / s"),
                  stop ("unknown format for Renishaw .txt files.")
                  )
  cols <- c  (cols, list (.wavelength = expression (Delta * tilde(nu) / cm^-1) ,
                          spc = "I / a.u."))

  first <- scan(file, nlines = 1, quiet = TRUE)
  ncol <- length (first)

  if (ncol == 0)
    return (new ("hyperSpec"))

  if (ncol != length (cols))
    stop (paste ("File has", ncol, "columns, while 'cols' gives", length (cols)))

  file <- file (file, "r")
  on.exit(close(file))

  fbuf <- matrix (scan (file, quiet = TRUE, nlines = nlines), ncol = ncol,
                  byrow = TRUE)

  ## wavelength axis
  wl <- rep (TRUE,  nrow (fbuf))
  for (i in seq_len (ncol (fbuf) - 2))
    wl [wl] <- fbuf [wl, i] == fbuf [1, i]

  wl <- fbuf[wl, ncol - 1]

  ## if the file is to be read in chunks
  ## try to find out how many lines it has
  if (is.null (nspc))
    if (nlines > 0){
      nspc <- wc (summary(file)$description, "lines")
      if (is.null (nspc))
        stop ("failed guessing nspc.")
      else {
        cat ("Counted", nspc[1,1], "lines or ")
        nspc <- nspc[1,1] / length (wl)
        cat (nspc, "spectra.\n")
      }
    } else {
      nspc <- nrow (fbuf) / length (wl)
    }

  data <- matrix (NA, ncol = ncol - 2, nrow = nspc)
  colnames (data) <- head (names (cols), -2)
  pos.data <- 0

  spc <- numeric (nspc * length (wl))
  pos.spc <- 0

  while (length (fbuf > 0)){
    if (nlines > 0) cat (".")
    spc [pos.spc + seq_len (nrow (fbuf))] <- fbuf [, ncol]
    pos.spc <- pos.spc + nrow (fbuf)

    tmp <- fbuf [fbuf[, ncol - 1] == wl [1], seq_len (ncol - 2), drop = FALSE]

    data [pos.data + seq_len (nrow (tmp)), ] <- tmp
    pos.data <- pos.data + nrow (tmp)

    fbuf <- matrix (scan (file, quiet = TRUE, nlines = nlines), ncol = ncol,
                    byrow = TRUE)

    if (length (fbuf > 0) & ! all(unique (fbuf[, ncol - 1]) %in% wl))
      stop ("Wavelengths do not correspond to that of the other chunks.",
            "Is the size of the first chunk large enough to cover a complete",
            "spectrum?")
  }
  if (nlines > 0) cat ("\n")

  spc <- matrix (spc, ncol = length (wl), nrow = nspc, byrow = TRUE)

  orderwl (new ("hyperSpec", spc = spc, data = as.data.frame (data),
                wavelength = wl, label = cols,
                log = list (short = short,
                  long = list (file = file, cols = I (cols),
                    nlines = nlines, nspc = nspc),
                  user = user, date = date
                )
           ))
}

