#################################################################################
###
###  y-cut.ranges.R - calculate pretty tick marks for cut axes (for plotspc with
###  multiple wl.ranges
###
###  Time-stamp: <Claudia Beleites on Wednesday, 2010-02-24 at 09:42:25 on cb>
###
#################################################################################

.cut.ticks <- function (start.ranges,
                       end.ranges,
                       offsets,
                       nticks){
  stopifnot (length (start.ranges) == length (end.ranges) &
             length (start.ranges) == length (offsets))

  ## if (length (start.ranges) == 1)       


  ## what part of the plot is occupied by each range?
  part <- abs (end.ranges - start.ranges) / (max (end.ranges) - min (start.ranges) - max (offsets))

  ## nice labels
  labels <- mapply (function (start, end, part) pretty (c (start, end), part * nticks + 1),
                    start.ranges, end.ranges, part,
                    SIMPLIFY = FALSE)

  ## user coordinates
  at <- mapply (`-`, labels, offsets, SIMPLIFY = FALSE)

  ## cut marks
  
  ## convert to device x in user coordinates
  start.ranges <- start.ranges - offsets
  end.ranges   <- end.ranges   - offsets

  delta <- start.ranges [-1] -  head (end.ranges, -1)

  cutmarks <-   head (end.ranges, -1) + delta / 2

  ## make sure that the ticks are not too close
  for (i in seq_along (delta)) {
    keep         <- labels [[i]] < end.ranges [i] + delta / 4
    at [[i]]     <- at     [[i]][keep]
    labels [[i]] <- labels [[i]][keep]

    keep             <- labels [[i + 1]] > start.ranges [i + 1] - delta / 4
    at [[i + 1]]     <- at     [[i + 1]][keep]
    labels [[i + 1]] <- labels [[i + 1]][keep]
  }
  
  list (labels = as.numeric (unlist (labels)),
        at     = as.numeric (unlist (at)),
        cut    = cutmarks)
}

