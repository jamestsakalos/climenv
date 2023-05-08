test_that("ce_plot() works", {

  # Set testing data ####

  # Create temporary file to supply to the ce_extract
  temp_path <- fs::file_temp(pattern = "Temp", tmp_dir = tempdir(), ext = "")

  # Create the required subdirectories
  dir.create(file.path(temp_path, "elev"), recursive = TRUE)
  on.exit(unlink(file.path(temp_path, "elev")))

  dir.create(file.path(temp_path, "clim/prec"), recursive = TRUE)
  dir.create(file.path(temp_path, "clim/tmax"), recursive = TRUE)
  dir.create(file.path(temp_path, "clim/tmean"), recursive = TRUE)
  dir.create(file.path(temp_path, "clim/tmin"), recursive = TRUE)
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
    terra::writeRaster(r, paste0(temp_path, "/clim/prec/", temp2[i]))
  }

  #* tmax ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44)
  temp2 <- paste0("tmax_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/clim/tmax/", temp2[i]))
  }

  #* tmin ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2
  temp2 <- paste0("tmin_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/clim/tmin/", temp2[i]))
  }

  #* tmean ####
  x <- c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) -
    c(c(43, 38, 33, 29, 25, 19.8, 17.01, 21, 25, 30, 37, 44) / 2) / 2
  temp2 <- paste0("tavg_", sprintf("%02d", 1:12), ".tif")
  for (i in seq_along(temp2)) {
    terra::values(r) <- c(rep(x[i], 50), rep(x[i] + 1, 50))
    terra::writeRaster(r, paste0(temp_path, "/clim/tmean/", temp2[i]))
  }

  # Create a polygon file from the raster
  terra::values(r) <- 1:100
  pol_py <- sf::st_as_sf(terra::as.polygons(r))
  pol_py$grp <- c(rep("low", 25), rep("high", 75))

  # Create a point file from the raster
  pol_pt <- sf::st_as_sf(terra::as.points(r))
  pol_pt$grp <- c(rep("low", 25), rep("high", 75))

  # py tests ####

  data <- ce_extract(dir_clim = file.path(temp_path, "clim"),
                     dir_elev = file.path(temp_path, "elev"),
                     location = pol_py, location_g = "grp",
                     c_source = "WorldClim", var = "ALL")

  p <- ce_plot(data = data, c_source = "WorldClim", location_g = "high",
                     type = "WL")

  vdiffr::expect_doppelganger("py test WL plot", p)

  p <- ce_plot(data = data, c_source = "WorldClim", location_g = "high",
               type = "H")

  vdiffr::expect_doppelganger("py test H plot", p)

  # pt tests ####

  data <- ce_extract(dir_clim = file.path(temp_path, "clim"),
                     dir_elev = file.path(temp_path, "elev"),
                     location = pol_pt, location_g = "grp",
                     c_source = "WorldClim", var = "ALL")

  p <- ce_plot(data = data, c_source = "WorldClim", location_g = "low",
               type = "WL")

  vdiffr::expect_doppelganger("pt test WL plot", p)

  p <- ce_plot(data = data, c_source = "WorldClim", location_g = "low",
               type = "H")

  vdiffr::expect_doppelganger("pt test H plot", p)

})
