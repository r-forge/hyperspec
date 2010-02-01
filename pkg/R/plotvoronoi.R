#################################################################################
###
###  plotvoronoi - plot spectral maps with irregular point pattern
###  
###  plots intensity or extra data column over 2 extra data columns

plotvoronoi <- function (object, model = spc ~ x * y,
                         use.tripack = TRUE, mix = TRUE, ...){
  if (use.tripack && mix)
      object@data <- object@data [sample (nrow (object)),]

  dots <- modifyList (list (object = object,
                            model = model,
                            panel = "panel.voronoi",
                            prepanel = "prepanel.default.levelplot",
                            pch = 19, cex = .25,
                            use.tripack = use.tripack),
                      list (...))
  do.call (plotmap, dots)
}
