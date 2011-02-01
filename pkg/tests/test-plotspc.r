context ("plotspc")

test_that("BARBITUATES", {
  spc <- do.call (collapse, barbituates [1:3])

  plotspc (spc, col = matlab.dark.palette (3), stacked = TRUE, lines.args = list (type = "h"))

}) 


