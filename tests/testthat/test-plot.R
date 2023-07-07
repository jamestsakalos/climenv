test_that("plot_XX() fails gracefully", {
  data("it_data", package = "climenv")
  expect_error(
    plot_h(data = it_data, geo_id = "ERROR"),
    "Invalid geo_id; Choices: MED, NEM"
  )
  expect_error(
    plot_wl(data = it_data, geo_id = "ERROR"),
    "Invalid geo_id; Choices: MED, NEM"
  )
})

test_that("ce_plot() works", {

  library("terra")
  library("sf")

  # Set testing data ####

  # Create temporary file to supply to the ce_extract
  temp_path <- tempfile()

  # Create the required subdirectories
  dir.create(file.path(temp_path, "elev"), recursive = TRUE)
  dir.create(file.path(temp_path, "prec"), recursive = TRUE)
  dir.create(file.path(temp_path, "tmax"), recursive = TRUE)
  dir.create(file.path(temp_path, "tavg"), recursive = TRUE)
  dir.create(file.path(temp_path, "tmin"), recursive = TRUE)
  on.exit(unlink(file.path(temp_path, "clim")), add = TRUE)

  # Create a empty raster serving as a base
  r <- terra::rast(ncol = 10, nrow = 10)

  # Modify the base Raster

  #* Elevation 100m ####
  terra::values(r) <- 1:100
  terra::writeRaster(r, paste0(temp_path, "/elev/srtm.tif"))

  #* Prec ####
  x <- c(5, 10, 15, 20, 25, 34.40666, 25, 20, 15, 10, 5, 0) * 8
  temp2 <- paste0("prec_", sprintf("%02d", 1:12), ".tif")

  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/prec/", temp2[i]))
  }

  #* tmax ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44)
  temp2 <- paste0("tmax_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/tmax/", temp2[i]))
  }

  #* tmin ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2
  temp2 <- paste0("tmin_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/tmin/", temp2[i]))
  }

  #* tavg ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) -
    c(c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2) / 2
  temp2 <- paste0("tavg_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/tavg/", temp2[i]))
  }

  # Create a polygon file from the raster
  terra::values(r) <- 1:100
  pol_py <- sf::st_as_sf(terra::as.polygons(r))
  pol_py$grp <- c(rep("low", 25), rep("high", 75))

  # py tests ####

  data <- ce_extract(path = file.path(temp_path),
                     location = pol_py, location_g = "grp",
                     c_source = "WorldClim", var = "all")

  lat0 <- data
  lat0$lat[1, 1] <- 0
  expect_error(plot_c(data = lat0, geo_id = "high"),
               "invalid latitude for `geo_id` must be positive or negative")

  # vdiffr is used only for testing so not required
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("py test WL plot",
                              plot_wl(data = data, geo_id = "high"))

  vdiffr::expect_doppelganger("py test H plot",
                              plot_h(data = data, geo_id = "high"))

  vdiffr::expect_doppelganger("py test c plot",
                              plot_c(data = data, geo_id = "high"))

  vdiffr::expect_doppelganger("py test c plot low",
                              plot_c(data = data, geo_id = "low"))

})
