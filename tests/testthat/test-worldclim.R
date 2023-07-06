test_that("worldclim() fails gracefully", {
  expect_error(worldclim(var = "stop"), "invalid `var`")
})

test_that("worldclim() downloads data", {
  skip_if_offline() # Requires connectivity
  skip_on_cran() # download is slow

  # CRAN policy: Packages should not write [anywhere] apart from the
  # R session’s temporary directory [...] and such usage should be cleaned up
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Single tile
  regentsPark <- matrix(byrow = TRUE, ncol = 2,
                        c(51.537, -0.150,
                          51.525, -0.145,
                          51.523, -0.156,
                          51.530, -0.167,
                          51.534, -0.163,
                          51.537, -0.150))
  regents <- sf::st_as_sf(as.data.frame(regentsPark), coords = 1:2)
  worldclim(out = tmp_dir, loc = regents, var = "tavg")
  expect_equal(
    file.exists(paste0(
      tmp_dir,
      "\\tavg\\wc2.1_30s_tavg_",
      formatC(1:12, width = 2, flag = "0"),
      ".tif")
    ),
    rep(TRUE, 12)
  )

  skip_on_ci() # Perhaps too slow?
  # Multiple tiles
  ni <- matrix(byrow = TRUE, ncol = 2,
               c(54.09, -6.26,
                 54.04, -6.68,
                 54.41, -6.97,
                 54.13, -7.39,
                 54.44, -8.17,
                 55.06, -7.24,
                 55.20, -6.05,
                 54.42, -5.44,
                 54.02, -6.06,
                 54.09, -6.26))
  niSF <- sf::st_as_sf(as.data.frame(ni), coords = 1:2)
  worldclim(out = tmp_dir, loc = niSF, var = "prec")
  expect_equal(
    file.exists(paste0(
      tmp_dir,
      "\\prec\\wc2.1_30s_prec_",
      formatC(1:12, width = 2, flag = "0"),
      ".tif")
    ),
    rep(TRUE, 12)
  )
})
