test_that("ce_extract() works", {
  # Set testing data ####

  # Create temporary file to supply to the ce_extract
  temp_path <- tempfile()
  on.exit(unlink(file.path(temp_path)), add = TRUE)

  # Create the required subdirectories
  dir.create(file.path(temp_path, "/elev"), recursive = TRUE)
  dir.create(file.path(temp_path, "/prec"), recursive = TRUE)
  dir.create(file.path(temp_path, "/tmax"), recursive = TRUE)
  dir.create(file.path(temp_path, "/tavg"), recursive = TRUE)
  dir.create(file.path(temp_path, "/tmin"), recursive = TRUE)

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

  # Create a point file from the raster
  pol_pt <- sf::st_as_sf(terra::as.points(r))
  pol_pt$grp <- c(rep("low", 25), rep("high", 75))

  # py tests ####

  #** No location group ####

  #* default warning when no location_g provided ####
  expect_silent(expect_warning(
    data_py <- ce_extract(
      path = file.path(temp_path),
      location = pol_py, location_g = NULL,
      c_source = "WorldClim", var = "all"
    ),
    "location_g must be one of: "
  ))

  #* length / names of output data.frames ####
  expect_named(data_py, c("tavg_m", "tavg_sd", "tmin_m", "abmt", "tmin_sd",
                          "tmax_m", "tmax_sd", "prec_m", "prec_sd",
                          "elev", "lat", "Readme"))
  #* nr. of observations ####
  expect_length(data_py$tavg_m[, 1], 100) # One for each value, pt
  #* Elev values are what I expect ####
  expect_equal(data_py$elev, data.frame(row.names = as.character(0:99),
                                        mean = c(1:100),
                                        stdev = 0))

  #** Location group ####

  #* !default messages when id provided ####
  expect_no_message({
    data_py <- ce_extract(
      path = file.path(temp_path),
      location = pol_py, location_g = "grp",
      c_source = "WorldClim", var = "all"
    )
  })

  #* length / names of output data.frames ####
  expect_named(data_py, c("tavg_m", "tavg_sd", "tmin_m", "abmt", "tmin_sd",
                          "tmax_m", "tmax_sd", "prec_m", "prec_sd",
                          "elev", "lat", "Readme"))
  #* nr. of observations ####
  expect_length(data_py$tavg_m[, 1], 2) # One for each group
  #* Elev values are what I expect ####
  expect_equal(data_py$elev,
               data.frame(row.names = as.factor(c("high", "low")),
                          mean = c(63, 13),
                          stdev = c(21.648710 + 2.51e-07, 7.211102 + 4.86e-07)))

  #** No elevation ####

  #* !default messages when id provided ####
  expect_no_message({
    data_py <- ce_extract(
      path = file.path(temp_path),
      location = pol_py, location_g = "grp",
      c_source = "WorldClim",
      var = c("tavg", "tmin", "tmax", "prec")
    )
  })

  #* length / names of output data.frames ####
  expect_named(data_py, c("tavg_m", "tavg_sd", "tmin_m", "abmt", "tmin_sd",
                          "tmax_m", "tmax_sd", "prec_m", "prec_sd", "lat",
                          "Readme"))

  #* nr. of observations ####
  expect_length(data_py$tavg_m[, 1], 2) # One for each group
  #* Tmin values are what I expect ####
  expect_named(data_py$tmin_m)
  expect_equal(data_py$tmin_m$Jan, c(22.16667 - 3.97e-06, 21.5))

  data_py <- ce_extract(
    path = file.path(temp_path),
    location = pol_py, location_g = "grp",
    c_source = "WorldClim", var = "prec"
  )

  #* length / names of output data.frames ####
  expect_named(data_py, c("prec_m", "prec_sd", "lat", "Readme"))

  # pt tests ####

  #** No location group ####

  #* default messages when no id provided ####
  expect_warning(
    data_pt <- ce_extract(
      path = file.path(temp_path),
      location = pol_pt, location_g = NULL,
      c_source = "WorldClim", var = "all"
    ),
    "location_g must be one of: "
  )

  #* length / names of output data.frames ####
  expect_named(data_pt, c("tavg", "tmin", "tmax",
                          "prec", "elev", "lat", "Readme"))

  #* nr. of observations ####
  expect_length(data_pt$tavg[, 1], 100) # One for each value, pt
  #* Elev values are what I expect ####
  expect_equal(data_pt$elev, data.frame(row.names = as.character(c(0:99)),
                                        mean = c(1:100)))

  #** Location group ####

  #* !default messages when id provided ####
  expect_no_message({
    data_pt <- ce_extract(
      path = file.path(temp_path),
      location = pol_pt, location_g = "grp",
      c_source = "WorldClim", var = "all"
    )
  })

  #* length / names of output data.frames ####
  expect_named(data_pt, c("tavg_m", "tavg_sd", "abmt", "tmin_m", "tmin_sd",
                          "tmax_m", "tmax_sd", "prec_m", "prec_sd",
                          "elev", "lat", "Readme"))

  #* nr. of observations ####
  expect_length(data_pt$tavg_m[, 1], 2) # One for each group
  #* Elev values are what I expect ####
  expect_equal(data_pt$elev,
               data.frame(row.names = as.character(c("high", "low")),
                          mean = c(63, 13),
                          stdev = c(21.79449, 7.359801)), tolerance = 1e-3)

  #** No elevation ####

  #* !default messages when id provided ####
  expect_no_message({
    data_pt <- ce_extract(
      path = file.path(temp_path),
      location = pol_pt, location_g = "grp",
      c_source = "WorldClim", var = c("tavg", "tmin", "tmax", "prec")
    )
  })

  #* length / names of output data.frames ####
  expect_named(data_pt, c("tavg_m", "tavg_sd", "abmt", "tmin_m", "tmin_sd",
                          "tmax_m", "tmax_sd", "prec_m", "prec_sd", "lat",
                          "Readme"))
  #* nr. of observations ####
  expect_length(data_pt$tavg_m[, 1], 2) # One for each group
  #* Tmin values are what I expect ####
  expect_named(data_pt$tmin_m)
  expect_equal(data_pt$tmin_m$Jan, c(22.16667, 21.5), tolerance = 1e-3)

  #* var error ####
  expect_error({
    ce_extract(
      path = file.path(temp_path),
      location = pol_pt, location_g = NULL,
      c_source = "WorldClim", var = "sdf"
    )
  })

  #* no var error ####
  expect_no_error({
    data_pt <- ce_extract(
      path = file.path(temp_path),
      location = pol_pt, location_g = "grp",
      c_source = "WorldClim", var = "prec"
    )
  })

  #* length / names of output data.frames ####
  expect_named(data_pt, c("prec_m", "prec_sd", "lat", "Readme"))

  # Testing helper functions

  # .spat_helper
  expect_error(.spat_helper(location = terra::vect(pol_pt)))
  expect_no_error({
    location <- .spat_helper(location = pol_pt)
  })

  # .dir_helper
  expect_error(.dir_helper(path = temp_path))
  expect_error(.dir_helper(var = "nonsense", path = temp_path))
  expect_no_error({
    .dir_helper(var = "tavg", path = temp_path)
  })

  # .location_helper
  location@data$id <- seq_along(location) - 1
  location_df <- data.frame(location)
  expect_no_error(
    .location_helper(location = pol_pt,
                     location_g = "grp",
                     location_df = location_df)
  )
  expect_warning(
    .location_helper(location = pol_pt,
                     location_g = NULL,
                     location_df = location_df)
  )

  # .c_source_helper
  expect_no_error(.c_source_helper(c_source = "CHELSA"))
  expect_no_error(.c_source_helper(c_source = "ChELSA"))
  expect_error(.c_source_helper(c_source = "wrong"))

  # .clim_extract
  expect_error(.clim_extract(
    which_clim = "tave",
    location = pol_pt,
    location_type = "SpatialPointsDataFrame",
    location_g = "grp",
    sd = TRUE,
    result,
    c_source = "WorldClim",
    path = temp_path
  ))

  expect_no_error(.clim_extract(
    which_clim = "tavg",
    location = pol_pt,
    location_type = "SpatialPointsDataFrame",
    location_g = "grp",
    sd = TRUE,
    result = NULL,
    c_source = "WorldClim",
    path = temp_path
  ))

})
