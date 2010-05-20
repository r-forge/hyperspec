#################################################################################
###
###  map.identify - identify spectra in map plot
###  

map.identify <- function (object, model = spc ~ x * y, voronoi = FALSE, ...){
  chk.hy (object)
  validObject (object)

  dots <- modifyList (list (object = object, model = model, ...),
                      list (subscripts = TRUE))
  if (voronoi) {
    dots <- modifyList (list (col = "black", border = "#00000080"),
                        dots)
    
    ## we need to mix the spectra, otherwise the voronoi plot does not work with 
    ## complete rectangular maps. mix keeps track of the reordering.
    dots$mix <- FALSE
    mix <- sample (nrow (object))
    dots$object <- object [mix]
    lattice <- do.call (plotvoronoi, dots)
    mix <- order (mix)
  } else {
    lattice <- do.call (plotmap, dots)
    mix <- row.seq (object)
  }

  print (lattice)
  trellis.focus ()

  ## PROBLEM: panel.identify does _not_ keep the order of the selection.
  ## End of input is right click, i.e. empty return value of panel.identify.
  ## "no observations within 18 points" or the like should not break the loop, though.
  ## Thus, count the warnings, and if the number increases, do not break.

  .nwarn <- function () 
    if (exists ("last.warning")) length (last.warning) else 0
  
  nwarn <- .nwarn ()
  res <- numeric (0)
  repeat {
    tmp <- panel.identify (x = lattice$panel.args.common$x [mix], 
                           y = lattice$panel.args.common$y [mix],
                           n = 1)

    if (length (tmp) == 0 && .nwarn () == nwarn)
      break
    else
      res <- c (res, tmp)

    nwarn <- .nwarn ()
  }

  res
}

