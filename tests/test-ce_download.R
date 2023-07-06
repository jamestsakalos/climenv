test_that("ce_download fails gracefully", {
  expect_error(ce_download(), "Set output directory")
  expect_error(ce_download(output_dir = ""), "Set spatial location")
  expect_error(ce_download(output_dir = "", location = "",
                           c_source = "BAD_VALUE"), "c_source must be")
  expect_error(
    expect_warning(
      ce_download(output_dir = "", location = "", var = "stop",
                  c_source = c("BAD_VALUE", "CHEL", "ALSO_BAD")),
      "Unrecognized value in c_source: BAD_VALUE, ALSO_BAD"),
    "invalid `var`")
  )
  ce_download("dir_does_not_exist")

})
