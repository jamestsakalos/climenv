polygon_py <- sf::st_polygon(
    list(cbind(long = c(161, 161, 154, 161),
               lat = c(-61, -49, -61, -61)))
  )
polygon_py <- sf::st_geometry(polygon_py)
sf::st_crs(polygon_py) <- "epsg:4326"
points <- terra::centroids(terra::vect(polygon_py))

polygon_py_sm <- sf::st_polygon(
  list(cbind(long = c(156, 156, 154, 156),
             lat = c(-61, -60, -61, -61)))
)
polygon_py_sm <- sf::st_geometry(polygon_py_sm)
sf::st_crs(polygon_py_sm) <- "epsg:4326"

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
    curlGetHeaders(server, timeout = 2),
    error = function(e) {
      if (length(grep("Connection timed out", e$message, fixed = TRUE))) {
        testthat::skip(paste("Could not connect to", server))
      }
    }
  )
}

test_that("elev() fails gracefully", {

  skip_if_server_offline("srtm.csi.cgiar.org")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  expect_error(elev(out = "", location = "", e_source = ""),
               "e_source must be ")

  # Invalid polygon
  flip_lat_long <- sf::st_polygon(list(cbind(
    lat = c(-61, -49, -61, -61),
    lng = c(161, 161, 154, 161)
  )))
  expect_error(
    elev(location = flip_lat_long),
    "bounding box falls outside supported latitudes"
  )

  # No data available in the oceans
  sea <- sf::st_as_sf(
    data.frame(lat = c(-59, -59, -58, -59),
               lng = c(-123, -124, -123, -123)), coords = 2:1)
  expect_snapshot(elev(tmp_dir, sea, "GEOdata", quiet = TRUE),
                  cran = TRUE, error = TRUE, scrub_progress_bars)

})

test_that("elev() downloads tiles not containing a vertex srtm", {

  skip_if_server_offline("srtm.csi.cgiar.org")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # Island example. Covers two srtm tiles (68_24 and 68_23), but the polygon
  # does not cover one tile not containing a vertex.
  island <- sf::st_polygon(
    list(cbind(lng = c(161, 161, 154, 161), lat = c(-61, -49, -61, -61))))

  # downloading the data for srtm
  expect_warning(
            geo_elev <- elev(tmp_dir, island, "GEOdata", quiet = TRUE),
    "Coordinate reference system not specified")

  thumb_0 <- terra::aggregate(geo_elev, fact = 20)

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_0, overwrite = TRUE,
      test_path("expected", "island_srtm_py.tif")
    )
  }

  expected <- terra::rast(test_path("expected", "island_srtm_py.tif"))
  expect_true(all.equal(terra::rast(thumb_0), terra::rast(expected)))

})

test_that("elev() downloads polygon from Mapzen", {

  skip_if_offline() # Requires connectivity. Automatically skips on CRAN.
  # CRAN policy: Packages should not write [anywhere] apart from the
  # R session’s temporary directory [...] and such usage should be cleaned up
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # download mapzen using a polygon ###
  elev(
    output_dir = tmp_dir, location = polygon_py_sm, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(mapzen_tile))

  # Check data matches expectation
  skip_if(!file.exists(mapzen_tile[1]))
  elev_mapzen <- terra::rast(mapzen_tile[1])
  thumb_1 <- terra::aggregate(elev_mapzen, fact = 64)

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_1, overwrite = TRUE,
      test_path("expected", "mapzen_py.tif")
    )
  }

  expected <- terra::rast(test_path("expected", "mapzen_py.tif"))
  expect_true(all.equal(terra::rast(thumb_1), terra::rast(expected)))

})

test_that("elev() downloads points from Mapzen", {

  skip_if_offline() # Requires connectivity. Automatically skips on CRAN.
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # download mapzen using points
  elev(
    output_dir = tmp_dir, location = points, e_source = "mapzen"
  )
  mapzen_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(mapzen_tile))

  # Check data matches expectation
  skip_if(!file.exists(mapzen_tile[1]))
  elev_mapzen <- terra::rast(mapzen_tile[1])
  thumb_2 <- terra::aggregate(elev_mapzen, fact = 64)

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_2, overwrite = TRUE,
      test_path("expected", "mapzen_pt.tif")
    )
  }

  expected <- terra::rast(test_path("expected", "mapzen_pt.tif"))
  expect_true(all.equal(terra::rast(thumb_2), terra::rast(expected)))

})

test_that("elev() downloads polygon from GeoData", {

  skip_if_server_offline("srtm.csi.cgiar.org")
  tmp_dir <- tempdir()
  on.exit(unlink(tmp_dir))

  # download mapzen using a polygon ###
  elev(
    output_dir = tmp_dir, location = polygon_py_sm, e_source = "geodata",
    quiet = TRUE
  )
  srtm_tile <- paste0(tmp_dir, "/elev/srtm.tif")
  expect_true(file.exists(srtm_tile))

  # Check data matches expectation
  skip_if(!file.exists(srtm_tile[1]))
  elev_srtm <- terra::rast(srtm_tile[1])
  thumb_3 <- terra::aggregate(elev_srtm, fact = 64)

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_3, overwrite = TRUE,
      test_path("expected", "srtm_py.tif")
    )
  }

  expected <- terra::rast(test_path("expected", "srtm_py.tif"))
  expect_true(all.equal(terra::rast(thumb_3), terra::rast(expected)))

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

  # Run this code manually to update the "Expected" value
  if (FALSE) {
    terra::writeRaster(
      thumb_4, overwrite = TRUE,
      test_path("expected", "srtm_pt.tif")
    )
  }

  expected <- terra::rast(test_path("expected", "srtm_pt.tif"))
  expect_true(all.equal(terra::rast(thumb_4), terra::rast(expected)))

})
