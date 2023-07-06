test_that("elev() fails gracefully", {
  expect_error(elev(out = "", location = "", e_source = ""),
               "e_source must be ")
})
