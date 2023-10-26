.clim_extract <- function(which_clim, location,
                          location_type, location_g,
                          sd, result, c_source, path) {

  # Pathway to the files
  file_path <- paste0(path, "/", which_clim)

  # raster stack
  cstack <- terra::rast(
    list.files(
      path = file_path, pattern = "\\.tif$",
      all.files = FALSE, full.names = TRUE
    )
  )

  # set the switch
  expr <- paste(location_type, sd)

  switch(
    expr,
    "SpatialPointsDataFrame FALSE" = {

      if (!which_clim == "elev") {
        names(cstack) <- month.abb
        extract_m <- terra::extract(cstack, terra::vect(location))
        extract_m$ID <- as.factor(location[[location_g]])
        colnames(extract_m)[1] <- "id"

        # Values for every point, not mean or sd.
        result[[glue::glue("{which_clim}")]] <- extract_m

      } else {

        # For elevation, no mean required, no grouping variable
        extract_m <- terra::extract(cstack, terra::vect(location))
        extract_m$location_g <- as.factor(location[[location_g]])

        dat <- extract_m[, c(3, 2)]
        names(dat) <- c("id", "mean")
        result[[glue::glue("{which_clim}")]] <- dat

      }

    },
    "SpatialPointsDataFrame TRUE" = {

      if (!which_clim == "elev") {
        names(cstack) <- month.abb
        extract_m <- terra::extract(cstack, terra::vect(location))
        extract_m$ID <- as.factor(location[[location_g]])
        names(extract_m)[names(extract_m) == "ID"] <- "location_g"

        extract_sd <- aggregate(. ~ location_g, extract_m,
                                FUN = function(x) {
                                  stats::sd(x, na.rm = TRUE)
                                })

        if (which_clim == "tmin") {

          extract_abmt <- aggregate(. ~ location_g, extract_m,
                                    FUN = function(x) {
                                      min(x, na.rm = TRUE)
                                    })

          result[["abmt"]] <- extract_abmt

        }

        extract_m <- aggregate(. ~ location_g, extract_m,
                               FUN = function(x) {
                                 mean(x, na.rm = TRUE)
                               })

        result[[glue::glue("{which_clim}_m")]] <- extract_m
        result[[glue::glue("{which_clim}_sd")]] <- extract_sd

      }else {

        extract_m <- terra::extract(cstack, terra::vect(location))
        extract_m$ID <- as.factor(location[[location_g]])
        names(extract_m)[names(extract_m) == "ID"] <- "location_g"

        extract_sd <- aggregate(. ~ location_g, extract_m,
                                FUN = function(x) {
                                  stats::sd(x, na.rm = TRUE)
                                })

        extract_m <- aggregate(. ~ location_g, extract_m,
                               FUN = function(x) {
                                 mean(x, na.rm = TRUE)
                               })

        colnames(extract_m) <- c("location_g", "mean")
        extract_m$stdev <- extract_sd$lyr.1

        result[[glue::glue("{which_clim}")]] <- data.frame(extract_m)

      }

    },
    "SpatialPolygonsDataFrame FALSE" = {

      if (!which_clim == "elev") {
        names(cstack) <- month.abb
        extract_m <- exactextractr::exact_extract(
          cstack, location, "mean", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(
          data.frame(id = 0:c(dim(extract_m)[1] - 1)), extract_m
        )
        result[[glue::glue("{which_clim}_m")]] <- extract_m

        if (which_clim == "tmin") {

          extract_m <- exactextractr::exact_extract(
            cstack, location, "min", progress = FALSE
          )
          colnames(extract_m) <- month.abb
          extract_abmt <- cbind(
            data.frame(id = 0:c(dim(extract_m)[1] - 1)), extract_m
          )
          result[["abmt"]] <- extract_abmt

        }

        extract_m <- exactextractr::exact_extract(
          cstack, location, "stdev", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(
          data.frame(id = 0:c(dim(extract_m)[1] - 1)), extract_m
        )
        result[[glue::glue("{which_clim}_sd")]] <- extract_m

      } else {

        # For elevation, zonal for polygons, no grouping variable
        extract_m <- exactextractr::exact_extract(
          cstack, location, c("mean", "stdev"), progress = FALSE
        )

        extract_m <- cbind(
          data.frame(id = as.factor(location[[location_g]]), extract_m)
        )

        result[[glue::glue("{which_clim}")]] <- extract_m

      }
    },
    "SpatialPolygonsDataFrame TRUE" = {

      if (!which_clim == "elev") {
        names(cstack) <- month.abb
        extract_m <- exactextractr::exact_extract(
          cstack, location, "mean", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        result[[glue::glue("{which_clim}_m")]] <- extract_m

        if (which_clim == "tmin") {

          extract_m <- exactextractr::exact_extract(
            cstack, location, "min", progress = FALSE
          )
          colnames(extract_m) <- month.abb
          extract_abmt <- cbind(
            data.frame(location_g = location$Name), extract_m
          )
          result[["abmt"]] <- extract_abmt

        }

        extract_m <- exactextractr::exact_extract(
          cstack, location, "stdev", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        result[[glue::glue("{which_clim}_sd")]] <- extract_m

      } else {

        extract_m <- exactextractr::exact_extract(
          cstack, location, c("mean", "stdev"), progress = FALSE
        )
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        result[[glue::glue("{which_clim}")]] <- extract_m

      }
    }
  )

  return(result)

}

.spat_helper <- function(location) {

  # Checks to make sure that location is the right class
  stopifnot(inherits(location, c("sf", "sfc")))

  # Convert sf locations to SP
  if (inherits(location, c("sf", "sfc"))) {
    location <- sf::as_Spatial(sf::st_zm(location))
  }
  return(location)
}

.c_source_helper <- function(c_source) {
  # Check if the c_source argument is correct.
  # Using partial, case-insensitive matching.
  if (is.na(pmatch(toupper(c_source), c("CHELSA", "WORLDCLIM")))) {
    stop("c_source must be either CHELSA, WorldClim")
  } else {
    return(c_source)
  }
}

.dir_helper <- function(var, path) {

  # Check if the properties of var are correct
  invisible(
    lapply(
      var, FUN = function(x) {
        stopifnot(x %in% c("all", "prec", "tmax", "tavg", "tmin"))
      }
    )
  )

  # Checks if the dir contains the correct folders.

  if ("all" %in% var) {

    # Checks to make sure that all climate folders are there
    if (!all(c("prec", "tmax", "tavg", "tmin", "elev") %in% list.files(path))) {
      stop("path must contain prec, tmax, tavg, tmin and elev subfolders")
    }

    # Checks that there are 12 rasters in each climate subfolder
    invisible(lapply(
      list.files(path)[list.files(path) %in% c("prec", "tavg", "tmax", "tmin")],
      FUN = function(x) {
        rast_nr <- length(list.files(paste(path, x, sep = "/")))
        if (!rast_nr == 12) {
          stop(paste(x, "contains", rast_nr, "files out of 12"))
        }
      }
    ))

    # Checks that there is 1 raster in each the elev folder
    invisible(lapply(
      list.files(path)[list.files(path) == "elev"],
      FUN = function(x) {
        rast_nr <- length(list.files(paste(path, x, sep = "/")))
        if (!rast_nr == 1) {
          stop(paste(x, "contains", rast_nr, "files out of 1"))
        }
      }
    ))

  }

  # Check each climate folder (if only one is supplied)
  if (sum(var %in% c("prec", "tmax", "tavg", "tmin")) > 0) {

    for (subfolder in seq_along(var)) {

      rast_nr <- length(list.files(paste0(path, "/", var[subfolder])))

      if (!rast_nr > 0) {
        stop(paste("Subfolder does not exist for", var[subfolder]))
      }

      if (!rast_nr == 12) {
        stop(paste(x, "contains", rast_nr, "files out of 12"))
      }

    }

  }

}

.location_helper <- function(location, location_g, location_df) {
  # Checks if the location argument is correct.

  # Assigns an 'id' if location_g is empty
  if (is.null(location_g)) {
    warning(
      paste(
        "location_g must be one of",
        paste(setdiff(colnames(location_df),  c("coords.x1", "coords.x2")),
              collapse = ", "), sep = ": "
      ), "\nDefaulting to a unique id for each polygon or point object"
    )
    location$Name <- location$id

    location_checks <- list("location" = location,
                            "location_g" = "id",
                            "location_df" = location_df,
                            "sd" = FALSE)
  }

  # If location is not empty then we need to check if it is correctly entered
  if (!is.null(location_g)) {

    # Creates a list of valid location names
    filter <- colnames(location_df)[
      !colnames(location_df) %in% c("coords.x1", "coords.x2", "optional")
    ]

    # Checks if there is a match and stops if there is a problem
    if (is.na(match(location_g, filter))) {
      stop(
        message(
          "location_g must be one of",
          paste((colnames(location_df)[
            !colnames(location_df) %in% c("coords.x1", "coords.x2")
          ]), collapse = ", "), sep = ": "
        )
      )
    }

    # If there are no problems lets export the data
    location_checks <- list(
      "location" = location,
      "location_g" = location_g,
      "location_df" = location_df,
      "sd" = TRUE # Calculate standard deviation when there are 'groups'
    )

  }

  return(location_checks)
}

#' ce_extract
#'
#' @description
#' Extracts climate and/or elevation data for generated over an area or at fixed
#' point/s.
#'
#' @template output_path_param
#' @template output_location_param
#' @template output_location_g_param
#' @template output_c_source_param
#' @template output_var_param
#'
#' @returns
#' Returns a list storing matrices containing the mean and standard deviation
#' of the climate and/or elevation data. Each column represents a month, each
#' row represents a feature of the \code{location} \code{sp}, \code{sf} polygon
#' or point object. Values returned are either degrees Celsius for (tmax, tavg,
#' tmin) or mm (prec).
#'
#' @author James L. Tsakalos and Martin R. Smith
#' @seealso The downloading ([`ce_download()`]), and the plotting
#' ([`plot_h()`] & [`plot_wl()`]) functions.
#'
#' @examples
#' # Create some random data
#'
#' # Create temporary file
#' temp_path <- tempfile()
#' on.exit(unlink(file.path(temp_path)), add = TRUE)
#'
#' # Create the required subdirectories
#' dir.create(file.path(temp_path, "/elev"), recursive = TRUE)
#' dir.create(file.path(temp_path, "/prec"), recursive = TRUE)
#' dir.create(file.path(temp_path, "/tmax"), recursive = TRUE)
#' dir.create(file.path(temp_path, "/tavg"), recursive = TRUE)
#' dir.create(file.path(temp_path, "/tmin"), recursive = TRUE)
#'
#' # Create a empty raster serving as a base
#' r <- terra::rast(ncol = 10, nrow = 10)
#'
#' # Modify the base Raster
#' #* Elevation 100m ####
#' terra::values(r) <- 1:100
#' terra::writeRaster(r, paste0(temp_path, "/elev/srtm.tif"))
#'
#'# create and save precipitation and temperature rasters ####
#'x <- c(5, 10, 15, 20, 25, 34, 25, 20, 15, 10, 5, 0) * 8
#'for (i in sprintf("%02d", 1:12)) {
#'terra::writeRaster(r, paste0(temp_path, paste0("/prec/prec_", i, ".tif")))
#'terra::writeRaster(r, paste0(temp_path, paste0("/tmax/tmax_", i, ".tif")))
#'terra::writeRaster(r, paste0(temp_path, paste0("/tmin/tmin_", i, ".tif")))
#'terra::writeRaster(r, paste0(temp_path, paste0("/tavg/tavg_", i, ".tif")))
#'}
#'
#' # Create a polygon file from the raster
#' terra::values(r) <- 1:100
#' pol_py <- sf::st_as_sf(terra::as.polygons(r))
#' pol_py$grp <- c(rep("low", 25), rep("high", 75))
#'
#' # Run the download function
#' ce_extract(
#'   path = temp_path,
#'   location = pol_py,
#'   location_g = "grp"
#' )
#'
#' @importFrom exactextractr exact_extract
#' @importFrom glue glue
#' @importFrom sp SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sf st_as_sf
#' @importFrom stats aggregate
#' @importFrom terra centroids crds extract rast vect
#' @importFrom dplyr %>% group_by summarize
#' @importFrom randomForest randomForest importance varImpPlot
#' @export
ce_extract <- function(
    path = NULL,
    location = NULL, location_g = NULL,
    c_source = "WorldClim", var = "all") {

  # Check if the properties of 'location' are correct.
  location <- .spat_helper(location)
  location@data$id <- seq_along(location) - 1
  location_df <- data.frame(location)

  # Check if the directory paths are correct
  .dir_helper(var, path)

  # Check the location arguments
  # Note returns a list that needs to be extracted.
  location_checks <- .location_helper(location, location_g, location_df)
  location <- location_checks[["location"]]
  location_g <- location_checks[["location_g"]]
  location_df <- location_checks[["location_df"]]
  sd <- location_checks[["sd"]]

  # This part sets the grouping column to location_g
  if (is.null(location_g) == FALSE) {
    if (!is.na(match(location_g, colnames(location_df)))) {
      location_df$Name <- as.factor(location_df[, location_g])

      if (inherits(location, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        # because `length(class(location))` may be > 1
        location <- sp::SpatialPointsDataFrame(location, location_df)
      } else {
        location <- sp::SpatialPolygonsDataFrame(location, location_df)
      }
      rm(location_df)
    }
  }

  # This part groups the shapefile according to location_g
  location_type <- class(location)[1]

  # set the switch
  expr <- paste(location_type, sd)
  switch(
    expr,
    "SpatialPointsDataFrame FALSE" = {
      location <- location %>%
        sf::st_as_sf(location) %>%
        dplyr::group_by("Name" = Name)
    },
    "SpatialPointsDataFrame TRUE" = {
      location <- location %>%
        sf::st_as_sf(location) %>%
        dplyr::group_by("Name" = Name)
    },
    "SpatialPolygonsDataFrame TRUE" = {
      location <- location %>%
        sf::st_as_sf(location) %>%
        dplyr::group_by("Name" = Name) %>%
        dplyr::summarize()
    }
  )

  # extracting the data ####
  if ("all" %in% var) {
    files <- c("tavg", "tmin", "tmax", "prec", "elev")
  } else {
    files <- var
  }

  result <- NULL
  result <- unlist(lapply(files, FUN = function(x) {
    .clim_extract(which_clim = x,
                  location = location,
                  location_type = location_type,
                  location_g = location_g,
                  sd = sd, result = result,
                  c_source = c_source,
                  path = path)
  }), recursive = FALSE)

  # If we need to adjust the values
  if (toupper(c_source) == "CHELSA") {
    toconvert <- names(result)[grepl("t", names(result))]
    for (i in toconvert) {
      result[[i]][, 2:13] <- (result[[i]][, 2:13] / 1)
    }
  }

  # Add the latitude
  switch(
    location_type,
    "SpatialPointsDataFrame" = {

      lat <- data.frame(
        "location_g" = location[[location_g]],
        "lat" = round(terra::vect(location)$coords.x2, 3)
      )

      ifelse(location_g == "id",
             {colnames(lat)[1] <- "id"},
             {colnames(lat)[1] <- "location_g"})

      if (location_g == "id") {
        colnames(lat)[1] <- "id"
      }

      if (!location_g == "id") {
        colnames(lat)[1] <- "location_g"
        lat <- aggregate(lat ~ location_g, data = lat, mean)
      }

      result[["lat"]] <- lat

    },
    "SpatialPolygonsDataFrame" = {

      if (!location_g == "id") {

        lat <- data.frame(
          "location_g" = location$Name,
          "lat" = round(
            terra::crds(terra::centroids(terra::vect(location)))[, 2], 3
          )
        )

        if (location_g == "id") {
          colnames(lat)[1] <- "id"
        }

        if (!location_g == "id") {
          colnames(lat)[1] <- "location_g"
        }

        result[["lat"]] <- lat

      } else {

        lat <- data.frame(
          "location_g" = location[[location_g]],
          "lat" = round(
            terra::crds(terra::centroids(terra::vect(location)))[, 2], 3
          )
        )

        if (location_g == "id") {
          colnames(lat)[1] <- "id"
        }

        if (!location_g == "id") {
          colnames(lat)[1] <- "location_g"
        }

        result[["lat"]] <- lat

      }

    }

  )

  # Set location_g or 'id' as the row.names for all data
  result <- lapply(result, FUN = function(x) {
    # Assigns the location_g column
    row.names(x) <- x[, colnames(x) %in% c("location_g", "id")]
    # removes the location_g column
    x <- x[, !grepl("_g$", names(x)), drop = FALSE] # Stops dropping to vector
    # removes the id column
    x <- x[, !grepl("id$", names(x)), drop = FALSE] # Stops dropping to vector
    return(x)
  })

  # To reduce the need to specify climate source when plotting.
  result$Readme <- paste("Contains data sourced from", c_source)

  return(result)
}
