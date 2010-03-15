context ("stacked.offsets")

test_that("BARBITUATES", {
  spc <- do.call (rbind.fill, barbituates [1:4])

  plotspc (spc, col = matlab.dark.palette (4), stacked = TRUE, lines.args = list (type = "h"))

}) 


