##' Import for Cytospec mat files
##' These functions allow to import .mat (Matlab V5) files written by Cytospec.
##' 
##' @param file The complete file name (or a connection to) the .mat file.
##' @param keys2data specifies which elements of the \code{Info} should be transferred into the extra data
##' @note This function is an ad-hoc implementation and subject to changes.
##' @return hyperSpec object if the file contains a single spectra block, otherwise a list with one
##' hyperSpec object for each block.
##' @author C. Beleites
##' @rdname read-cytomat
##' @seealso \code{R.matlab::readMat}
##' @export
##' @keywords IO file
read.cytomat <- function (file, keys2data = FALSE) {
  if (! require ("R.matlab"))
      stop ("package 'R.matlab' needed.")
  
  tmp <- readMat(file)
  
  ## read spectra matrix
  spc <- tmp$C
  d <- dim (spc)
  
  ## get wavelength information
  fileinfo<-(tmp$Info[[1]])
  lwn <- as.numeric (fileinfo [grep ("LWN", fileinfo) - 1])
  hwn <- as.numeric (fileinfo [grep ("VWN", fileinfo) - 1])
  wn <- seq (lwn, hwn, length.out = dim (spc)[3])

  ## x + y coordinates
  y <- rep (1 : d [1], each = d [2])
  x <- rep (1 : d [2], d [1])

  extra.data <- data.frame (x = x, y = y, file = file)
  
  blocks <- d [4]

  if (is.na (blocks)){                  # single block file
    .block2hyperSpec (spc, extra.data, wn)
   } else { # multi-block file 
     res <- list ()
     for (b in blocks) 
         res [[b]] <-     .block2hyperSpec (spc, extra.data, wn, b)
     res
  }
}

.block2hyperSpec <- function (spc, df, wn, block = 1) {

  d <- dim (spc)
  dim (spc) <- c (d [1] * d[2], d [3])

  df$block <- block
  
  new ("hyperSpec", spc = spc, wavelength = wn, data = df)
}
