test_that("worldclim() fails gracefully", {
  expect_error(worldclim(var = "stop"), "invalid `var`")
})
