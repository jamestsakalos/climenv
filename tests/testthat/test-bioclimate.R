test_that("bioclimate() fails gracefully", {
  expect_error(bioclimate(matrix(1:11, 11), matrix(1:11)),
               "12 months")
  expect_error(bioclimate(matrix(1:24, 12), matrix(1:12)),
               "should have the same dimensions")
})
