context ("stacked.offsets")

test_that("BARBITUATES", {
  spc <- do.call (collapse, barbituates [1:4])
  stacked.offsets (spc)
}) 

test_that("add", {
  spc <- new ("hyperSpec", spc = matrix (c (0, 0, 2, 1 : 3), nrow = 3))
  stacked.offsets (spc, add.factor = 0)
  stacked.offsets (spc, add.factor = 1)
  stacked.offsets (spc, add.sum = 1)
}) 

test_that("zero", {
  stacked.offsets (flu, min.zero = TRUE)
})


