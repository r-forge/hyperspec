library (hyperSpec)
library (spatstat)

hf <- as.hyperframe (chondro@data)

hf

mean_or_colMean <- function (x){if (! is.null (dim (x))) colMeans (x) else mean (x)}

mean_or_colMean <- function (x, ...){
  if (! is.null (dim (x)))
    apply (x, 2, quantile, ...)
  else
    quantile (x, ...)
}

system.time (lapply (chondro$.., hyperSpec:::.na.if.different))
system.time (apply (chondro$.., 2, hyperSpec:::.na.if.different))

system.time (apply (chondro@data$spc, 2, quantile, probs = c (.16, .5, .84)))

system.time (apply (chondro, 2, quantile, probs = c (.16, .5, .84)))

myfun <- function (x){
  if (is.numeric (x))
    if (is.matrix (x))
      apply (x, 2, quantile, c (.16, .5, .84))
    else
      quantile (x, c (.16, .5, .84))
  else
    rep (NA, 3)
}
system.time (tmp <- sapply (chondro@data, myfun))
dummy <- tmp
attr (dummy, "class") <- data.frame
attr (dummy, "row.names") <- 1:2

dummy


`%dim%` <- function (x, y, ...)
  function (X, ...){
    if (is.null (dim (X)))
      x (X, ...)
    else
      y (X, ...)
  }

`%col%` <- function (x, y, ...)
  function (X, ...){
    if (is.null (dim (X)))
      x (X, ...)
    else
      apply (X, 2, y, ...)
  }

`%row%` <- function (x, y, ...)
  function (X, ...){
    if (is.null (dim (X)))
      x (X, ...)
    else
      apply (X, 1, x, ...)
  }

l <- lapply (dummy, names %dim% rownames)
l <- l [!sapply (l, is.null)]
l <- as.matrix (as.data.frame (l))
if (all (sweep (l [, -1], 1, l [, 1], `==`)))
  rownames (tmp) <- l [, 1]
else
  rownames (tmp) <- 1 : 
