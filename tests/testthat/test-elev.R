test_that("elev() fails gracefully", {

  expect_error(elev(out = "", location = "", e_source = ""),
               "e_source must be ")

  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))
  # No data available in the oceans
  sea <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -124, -123, -123)), coords = 2:1)
  # elev(tmp_dir, tile50)
  expect_warning(
    expect_error(elev(tmp_dir, sea, "GEOdata"), "No data downloaded"),
    "Could not download srtm_12_24"
  )

  expect_error(
    expect_warning(
      elev(),
      "Error in elev() : argument location is missing, with no default"
    ))

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

  expect_error(
    expect_warning(
      elev(location = flip_lat_long),
      "Error: check that the location has been projected (epsg: 4326)"
    ))

})

test_that("elev()", {
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Two squares, 68_24 and 68_23, cover
  #   Latitude south: 50-60
  #   Longitude east: 155-160
  # We should be able to download these squares even if our target area
  # overlaps non-existent neighbouring squares

  island <- sf::st_as_sf(
    data.frame(lat = c(-61, -59, -51, -49, -61, -61),
               lng = c(161, 159, 159, 161, 154, 161)), coords = 2:1)

  # In progress: the below will replace the above.
  island <- sf::st_polygon(
    list(cbind(lat = c(-61, -49, -61, -61), lng = c(161, 161, 154, 161))))
  # This polygon covers a tile that does not contain a vertex.
  # Should this tile be downloaded too?

  # elev(tmp_dir, island)
  expect_warning(
    geoElev <- elev(tmp_dir, island, "GEOdata"),
    "Could not download srtm_69_22")

  skip_if_not_installed("vdiffr")
  library("terra")
  vdiffr::expect_doppelganger("geo-elev", function() plot(geoElev))
})
