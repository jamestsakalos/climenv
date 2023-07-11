test_that("elev() fails gracefully", {

  expect_error(elev(out = "", location = "", e_source = ""),
               "e_source must be ")

  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # No data available in the oceans
  sea <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -124, -123, -123)), coords = 2:1)
  expect_snapshot(elev(tmp_dir, sea, "GEOdata"), cran = TRUE, error = TRUE)

  # This is testing R's functionality, rather than our packages, so does
  # not need to be included in this package's test suite;
  # we do not handle the case where no parameters are specified.
  expect_error(
    expect_warning(
      elev(),
      "Error in elev() : argument location is missing, with no default"
    ))

  flip_lat_long <- sf::st_polygon(list(cbind(
    lat = c(-61, -49, -61, -61),
    lng = c(161, 161, 154, 161)
  )))
  expect_error(
    elev(location = flip_lat_long),
    "bounding box falls outside supported latitudes"
  )

})

test_that("elev()", {
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Two squares, 68_24 and 68_23, cover
  #   Latitude south: 50-60
  #   Longitude east: 155-160
  # We should be able to download these squares even if our target area
  # overlaps non-existent neighbouring squares
  island <- sf::st_polygon(
    list(cbind(lng = c(161, 161, 154, 161), lat = c(-61, -49, -61, -61))))
  # This polygon covers a tile that does not contain a vertex.
  # This tile should be downloaded too

  expect_snapshot(geo_elev <- elev(tmp_dir, island, "GEOdata", quiet = TRUE),
                  cran = TRUE)
  # Expect the warnings:
  # "Coordinate reference system not specified",
  # "Could not download srtm_6._2."

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("geo-elev", function() plot(geo_elev))
})
