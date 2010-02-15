###-----------------------------------------------------------------------------
###
### as.wide.df
###
### TODO: look for other matrices/arrays to expand

as.wide.df <- function (x) {
  .is.hy (x)
  validObject (x)

  ispc <- match ("spc", colnames (x@data))

  ## logical indexing creates n by 0 data.frame that can be cbound, thus
  ## avoiding trouble with empty or 0 x 0 data.frames:
  before <- seq_len (ncol (x@data)) < ispc
  after <- seq_len (ncol (x@data)) > ispc

  ## colnames should be preserved

  cols <- c (colnames (x@data)  [before],
             colnames (x@data$spc),
             colnames (x@data) [after])

  x <- cbind (x@data [, before],
              as.data.frame (unclass (x@data [, ispc])),
              x@data [, after])
  colnames (x) <- cols
  x
}

