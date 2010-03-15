context ("stacked.offsets")

test_that("BARBITUATES", {
  spc <- do.call (rbind.fill, barbituates [1:4])
  stacked.offsets (spc)
}) 

test_that("zero", {
  spc <- new ("hyperSpec", spc = matrix (c (0, 0, 2, 1 : 3), nrow = 3))

  plot (spc)
  expect_that (stacked.offsets (spc, add.factor = 0), equals = 
  stacked.offsets (spc, add.factor = 1)
  stacked.offsets (spc, add.sum = 1)
}) 

test_that("zero", {
  stacked.offsets (flu, min.zero = TRUE)
})


