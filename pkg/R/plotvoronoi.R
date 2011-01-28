#################################################################################
###
###  plotvoronoi - plot spectral maps with irregular point pattern
###  
###  plots intensity or extra data column over 2 extra data columns

plotvoronoi <- function (object, model = spc ~ x * y,
                         use.tripack = FALSE, mix = FALSE, ...){
  if (!require (latticeExtra))
   stop ("package latticeExtra is needed for Voronoi plots.")

  if (use.tripack){
    if (!require (tripack))
      stop ("package tripack requested but not available.")
  } else {
    if (!require (deldir))
      stop ("package deldir requested but not available.")
  }
 
  if (use.tripack && mix)
      object@data <- object@data [sample (nrow (object)),]

  dots <- modifyList (list (object = object,
                            model = model,
                            panel = "panel.voronoi",
                            prepanel = "prepanel.default.levelplot",
                            pch = 19, cex = .25,
                            col.symbol = "#00000020",
                            border = "#00000020",
                            use.tripack = use.tripack),
                      list (...))
  do.call (plotmap, dots)
}
