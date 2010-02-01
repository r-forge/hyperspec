#################################################################################
###
###  plotmap - plot spectral maps
###  
###  plots intensity or extra data column over 2 extra data columns

plotmap <- function (object, model = spc ~ x * y,
                     func = mean, func.args = list (), ...){
  .is.hy (object)
  validObject (object)

  if (nwl (object) > 1)
    object <- do.call (apply, c (list (object, 1, func), func.args))
  
  dots <- modifyList (list (aspect = "iso"),
                      list (...))
                       
  dots <- c (list (x = model, data = object), dots)

  do.call(.levelplot, dots)
}

