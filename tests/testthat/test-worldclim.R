test_that("worldclim() fails gracefully", {
  expect_error(
    expect_warning(worldclim(var = "stop", res = 1),
                   "Invalid value of `resolution`")
    , "invalid `var`")

  flipLatLong <- sf::st_as_sf(
    data.frame(lat = c(-72, -73, -73, -72),
               lng = c(-156, -156, -157, -156)), coords = 1:2)
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))
  expect_null(expect_warning(
    worldclim(out = tmp_dir, loc = flipLatLong, var = "prec"),
    "Could not map all coordinates to tiles"))
})

test_that("worldclim() downloads data", {
  skip_if_offline() # Requires connectivity
  skip_on_cran() # download is slow

  # CRAN policy: Packages should not write [anywhere] apart from the
  # R session’s temporary directory [...] and such usage should be cleaned up
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Smallest download available is tile_50_wc2.1_30s_elev.tif: 134K
  # Smallest climate download is tile_50_wc2.1_30s_prec.tif: 407K
  tile50 <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -124, -123, -123)), coords = 2:1)

  # Obtain raster files
  worldclim(out = tmp_dir, loc = tile50, var = "prec")
  tileFiles <- paste0(tmp_dir, "\\prec\\wc2.1_30s_prec_",
                      formatC(1:12, width = 2, flag = "0"), ".tif")
  expect_equal(file.exists(tileFiles), rep(TRUE, 12))

  # Check data matches expectation
  jan50 <- terra::rast(tileFiles[1])
  thumb <- terra::aggregate(jan50, fact = 64)
  expected <- terra::rast(system.file("tests/testthat/expected/jan50.tif",
                                      package = "climenv"))
  expect_true(all.equal(rast(thumb), rast(expected)))


  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb, overwrite = TRUE,
      paste0(system.file("tests/testthat/expected", package = "climenv"),
             "/jan50.tif")
      )
  }


  # Multiple tiles: spanning 49 & 50 will give smallest download size (~280KB)
  south <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -174, -123, -123)), coords = 2:1)

  # Obtain raster data
  worldclim(out = tmp_dir, loc = south, var = "elev")
  southFile <- paste0(tmp_dir, "\\elev\\wc2.1_30s_elev_01.tif")
  expect_true(file.exists(southFile))

  # Check data matches expectation
  southElev <- terra::rast(southFile)
  thumb <- terra::aggregate(southElev, fact = 64)
  expected <- terra::rast(system.file("tests/testthat/expected/southElev.tif",
                                      package = "climenv"))
  expect_true(all.equal(rast(thumb), rast(expected)))


  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb, overwrite = TRUE,
      paste0(system.file("tests/testthat/expected", package = "climenv"),
             "/southElev.tif")
    )
  }
})