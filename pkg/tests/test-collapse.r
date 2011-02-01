context("collapse.hyperSpec")

test_that("BARBITUATES", {
  new <- do.call (collapse, barbituates)
  wl <- sort (unique (unlist (lapply (barbituates, slot, "wavelength"))))
  expect_that (sort (wl (new)), equals (wl))

  expect_that (sort (unlist (lapply (barbituates, function (x) as.numeric (x@data$spc)))),
               equals (sort (unclass (new$spc[! is.na (new$spc)]))))
})

