trellis.factor.key <- function (f, levelplot.args = list ()) {
  at <-  seq (0, nlevels (f)) + .5
  
  if (is.null (levelplot.args$col.regions))
    cols <- level.colors (seq_along (levels (f)), at)
  else
    cols <- level.colors (seq_along (levels (f)), at, levelplot.args$col.regions)
    
  modifyList (list (at = at,
                    col.regions = cols, 
                    colorkey = list (lab = list (at = seq_along (levels (f)),
                                                 lab = levels (f)))),
              levelplot.args)
  
}
