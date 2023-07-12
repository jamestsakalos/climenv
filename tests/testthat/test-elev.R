polygon <- sf::st_polygon(
    list(cbind(long = c(161, 161, 154, 161),
               lat = c(-61, -49, -61, -61)))
  )
polygon <- sf::st_geometry(polygon)
sf::st_crs(polygon) <- "epsg:4326"
points <- terra::centroids(terra::vect(polygon))


scrub_progress_bars <- function(x) {
  progress_bars <- grep("^[\\|\\-=\\s]*$", x, perl = TRUE)
  if (length(progress_bars)) {
    x[-progress_bars]
  } else {
    x
  }
}

skip_if_server_offline <- function(server) {
  # Preferred to testthat::skip_if_offline as this runs on CRAN
  # Thus we can expect notice of any breaking changes to imported packages
  tryCatch(
    curlGetHeaders(server, timeout = 1),
    error = function(e) {
      if (length(grep("Connection timed out", e$message, fixed = TRUE))) {
        testthat::skip(paste("Could not connect to", server))
      }
    }
  )
}

test_that("elev() fails gracefully", {

  expect_error(elev(out = "", location = "", e_source = ""),
               "e_source must be ")

  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # When location is not supplied
  # This is testing R's functionality, rather than our packages, so does
  # not need to be included in this package's test suite;
  # we do not handle the case where no parameters are specified.
  expect_error(
    expect_warning(
      elev(),
      "Error in elev() : argument location is missing, with no default"
    ))

  # Invalid polygon
  flip_lat_long <- sf::st_polygon(list(cbind(
    lat = c(-61, -49, -61, -61),
    lng = c(161, 161, 154, 161)
  )))
  expect_error(
    elev(location = flip_lat_long),
    "bounding box falls outside supported latitudes"
  )

  skip_if_server_offline("srtm.csi.cgiar.org")
  # No data available in the oceans
  sea <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -124, -123, -123)), coords = 2:1)
  expect_snapshot(elev(tmp_dir, sea, "GEOdata", quiet = TRUE),
                  cran = TRUE, error = TRUE, scrub_progress_bars)

})

test_that("elev()", {
  skip_if_server_offline("srtm.csi.cgiar.org")

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

  expect_warning(
    expect_warning(
      expect_warning(
        expect_warning(
          expect_warning(
            geo_elev <- elev(tmp_dir, island, "GEOdata", quiet = TRUE),
            "Could not download srtm_6._2."),
          "Could not download srtm_6._2."),
        "Could not download srtm_6._2."),
      "Could not download srtm_6._2."),
    "Coordinate reference system not specified")

  # TODO JS to investigate:
  # elev.R:172   terra::writeRaster(srtm_mosaic, filename = file_path, [...]
  # throws
  #  Error: [writeRaster] there are no cell values
  expect_snapshot(mz_elev <- elev(tmp_dir, island), cran = TRUE)

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("geo-elev", function() terra::plot(geo_elev))
})

test_that("elev() downloads polygon from Mapzen", {
  skip_if_offline() # Requires connectivity. Automatically skips on CRAN.

  # CRAN policy: Packages should not write [anywhere] apart from the
  # R session’s temporary directory [...] and such usage should be cleaned up
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # polygon and mapzen tiles ####
  elev(
    output_dir = tmp_dir, location = polygon, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(mapzen_tile))

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
})
test_that("elev() downloads points from Mapzen", {
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  elev(
    output_dir = tmp_dir, location = points, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(mapzen_tile))

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
})


test_that("elev() downloads polygon from GeoData", {
  skip_if_server_offline("srtm.csi.cgiar.org")

  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # polygon and geodata ####
  elev(
    output_dir = tmp_dir, location = polygon, e_source = "geodata", quiet = TRUE
  )
  srtm_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(srtm_tile))

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
})


test_that("elev() downloads points from GeoData", {
  skip_if_server_offline("srtm.csi.cgiar.org")

  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  elev(
    output_dir = tmp_dir, location = points, e_source = "geodata", quiet = TRUE
  )
  srtm_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(srtm_tile))

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
