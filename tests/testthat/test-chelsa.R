test_that("chelsa() fails gracefully", {
  expect_error(chelsa(var = "stop"), "invalid `var`")
})
