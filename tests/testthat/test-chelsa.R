test_that("chelsa() fails gracefully", {
  expect_error(chelsa(out = "", loc = "", var = "stop"), "invalid `var`")
})
