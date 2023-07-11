test_that("elev() fails gracefully", {

  # When location is not supplied
  expect_error(
    expect_warning(
      elev(),
      "Error in elev() : argument location is missing, with no default"
    ))

  # When a user accidentally puts in an invalid polygon
  flip_lat_long <- sf::st_polygon(
    list(cbind(lat = c(-61, -49, -61, -61), lng = c(161, 161, 154, 161))))

  expect_error(
    expect_warning(
      elev(location = flip_lat_long),
      "Error in elev(location = flip_lat_long) :
  bounding box of location has potentially an invalid value range"
    ))

  flip_lat_long <- sf::st_polygon(
    list(cbind(long = c(161, 161, 154, 161),
               lat = c(-61, -49, -61, -61)))
  )

  # When a user has not made sure that the polygon is projected.
  expect_error(
    expect_warning(
      elev(location = flip_lat_long),
      "Error: check that the location has been projected (epsg: 4326)"
    ))
})

test_that("elev() downloads data", {

  skip_if_offline() # Requires connectivity
  skip_on_cran() # download is slow

  # CRAN policy: Packages should not write [anywhere] apart from the
  # R session’s temporary directory [...] and such usage should be cleaned up
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Make a polygon
  location <- sf::st_polygon(
    list(cbind(long = c(161, 161, 154, 161),
               lat = c(-61, -49, -61, -61)))
  )
  location <- sf::st_geometry(location)
  sf::st_crs(location) <- "epsg:4326"

  # polygon and mapzen tiles ####
  elev(
    output_dir = tmp_dir, location = location, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_equal(file.exists(mapzen_tile), TRUE)

  # Check data matches expectation
  skip_if(!file.exists(mapzen_tile[1]))
  elev_mapzen <- terra::rast(mapzen_tile[1])
  thumb_1 <- terra::aggregate(elev_mapzen, fact = 64)
  expected <- terra::rast(test_path("expected", "mapzen_py.tif"))
  expect_true(all.equal(rast(thumb_1), terra::rast(expected)))

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_1, overwrite = TRUE,
      test_path("expected", "mapzen_py.tif")
    )
  }

  unlink(tmp_dir)
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # polygon and geodata ####
  elev(
    output_dir = tmp_dir, location = location, e_source = "geodata"
  )
  srtm_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_equal(file.exists(srtm_tile), TRUE)

  # Check data matches expectation
  skip_if(!file.exists(srtm_tile[1]))
  elev_srtm <- terra::rast(srtm_tile[1])
  thumb_2 <- terra::aggregate(elev_srtm, fact = 64)
  expected <- terra::rast(test_path("expected", "srtm_py.tif"))
  expect_true(all.equal(rast(thumb_2), terra::rast(expected)))

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_2, overwrite = TRUE,
      test_path("expected", "srtm_py.tif")
    )
  }

  # Now for points
  location <- terra::centroids(terra::vect(location))

  unlink(tmp_dir)
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # I expect error because the package does not accept spatvectors
  expect_error(elev(output_dir = tmp_dir,
                    location = location))

  # sf class is okay.
  location <- sf::st_as_sf(location)

  # point and mapzen tiles ####
  elev(
    output_dir = tmp_dir, location = location, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_equal(file.exists(mapzen_tile), TRUE)

  # Check data matches expectation
  skip_if(!file.exists(mapzen_tile[1]))
  elev_mapzen <- terra::rast(mapzen_tile[1])
  thumb_3 <- terra::aggregate(elev_mapzen, fact = 64)
  expected <- terra::rast(test_path("expected", "mapzen_pt.tif"))
  expect_true(all.equal(terra::rast(thumb_3), terra::rast(expected)))

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_3, overwrite = TRUE,
      test_path("expected", "mapzen_pt.tif")
    )
  }

  unlink(tmp_dir)
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # polygon and geodata
  elev(
    output_dir = tmp_dir, location = location, e_source = "geodata"
  )
  srtm_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_equal(file.exists(srtm_tile), TRUE)

  # Check data matches expectation
  skip_if(!file.exists(srtm_tile[1]))
  elev_srtm <- terra::rast(srtm_tile[1])
  thumb_4 <- terra::aggregate(elev_srtm, fact = 64)
  expected <- terra::rast(test_path("expected", "srtm_pt.tif"))
  expect_true(all.equal(terra::rast(thumb_4), terra::rast(expected)))

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_4, overwrite = TRUE,
      test_path("expected", "srtm_pt.tif")
    )
  }

})
