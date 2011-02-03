

fortify.hyperSpec <- function (model, data, ...){
  browser ()
  as.long.df (sample (model, 10), rownames = TRUE)
}



### attempts to provide cut axis
## questions:
## - remove connecting line
## - combine with other transformation, e.g. reverse

cutfun <- function(x) {
  x [x > 1800 & x < 2800] <- NA
  x [isTRUE (x > 1800)] <- x - 900

  x
}

TransCut <- Trans$new ("cut",
                       function(x) {
                         x [x > 1800 & x < 2800] <- NA
                         x [isTRUE (x > 1800)] <- x - 900
                         
                         x
                       },
                       
                       function(x) {
                         x [x > 1900] <- x + 900
                         x
                       }
                       )

ScaleCut <-    proto(ScaleContinuous,
  desc = "Position scale, log10 transformed",
  tr_default = Trans$find("cut"),
  objname = "cut",
  doc=FALSE,
  examples=function(.) {}
)

scale_x_cut <- ScaleCut$build_accessor(list(variable = "\"x\""))
