test_that("shrinkage coef base case", {
  expect_equal(shrinkage_coef_non_overlapping(1, 0.5), 0.5)
})
