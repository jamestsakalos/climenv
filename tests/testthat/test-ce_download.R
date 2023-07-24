test_that("ce_download fails gracefully", {
  expect_error(ce_download(), "Set output directory")
  expect_error(ce_download(output_dir = ""), "Set spatial location")
  expect_warning(ce_download(output_dir = "", location = "", e = NULL,
                             c_source = "BAD_VALUE"),
                 "Unrecognized value in c_source")
  expect_error(
    expect_warning(
      ce_download(output_dir = "", location = "", var = "stop",
                  c_source = c("BAD_VALUE", "CHEL", "ALSO_BAD")),
      "Unrecognized value in c_source: BAD_VALUE, ALSO_BAD"
    ),
    "invalid `var`"
  )
  expect_error(
    expect_warning(
      ce_download(output_dir = "", location = "", var = "stop",
                  c_source = c("BAD_VALUE", "W", "ALSO_BAD")),
      "Unrecognized value in c_source: BAD_VALUE, ALSO_BAD"
    ),
    "invalid `var`"
  )
  expect_error(
    expect_warning(
      ce_download(out = "", loc = "", c_source = "", e_source = "error"),
      "Unrecognized value in c_source"
    ),
    "e_source must be"
  )
})

test_that("ce_download() calls required functions", {
  # Imperfect checks: should check that ONLY required functions are called
  expect_error(
    ce_download(output_dir = "", location = "", var = "stop", c_source = "c"),
    "invalid `var`"
  )
  expect_error(
    ce_download(output_dir = "", location = "", var = "stop", c_source = "w"),
    "invalid `var`"
  )
  # TODO James: if users should be able to recover JUST elevation data using
  # ce_download (which seems desirable for simplicity), then this should be
  # supported without an error message; i.e. we should support c_source = NULL.
  expect_error(
    expect_warning(
      ce_download(out = "", loc = "", c_source = "none", e_source = ""),
      "Unrecognized value in c_source"
    ),
    "e_source must be"
  )
})
