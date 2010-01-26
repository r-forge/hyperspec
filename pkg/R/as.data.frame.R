###-----------------------------------------------------------------------------
###
### as.data.frame
###

setMethod ("as.data.frame",
           signature (x = "hyperSpec", row.names = "missing", optional = "missing"),
           function (x, ...){
             validObject (x)

             x@data
           })

###-----------------------------------------------------------------------------
###
### as.long.df
###
### TODO: look for other matrices/arrays to expand

as.long.df <- function (x, rownames = FALSE) {
  .is.hy (x)
  validObject (x)

  ispc <- match ("spc", colnames (x@data))

  if (nwl (x) == 0) {
    tmp <- cbind (data.frame (.wavelength = rep (NA, nrow (x)),
                              spc = rep (NA, nrow (x))),
                  x@data [, -ispc, drop = FALSE])
  } else {
    tmp <- x@data [rep (seq (nrow (x)), nwl (x)), -ispc, drop = FALSE]

    tmp <- cbind (data.frame (.wavelength = rep (x@wavelength, each = nrow (x)),
                              spc = as.numeric (x [[]])),
                  tmp)
  }

  if (rownames)
    tmp <- data.frame (.rownames = as.factor (rep (rownames (x),
                         length.out = nrow (tmp))),
                       tmp)

  tmp
}


###-----------------------------------------------------------------------------
###
### as.matrix
###

setMethod ("as.matrix", "hyperSpec", function (x, ...){
  validObject (x)

  x@data$spc
})

