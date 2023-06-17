.clim_extract <- function(which_clim, location,
                          location_type, location_g,
                          sd, result, c_source, dir_clim, dir_elev) {

  file_path <- paste0(dir_clim, "/", which_clim)

  if (isTRUE(which_clim == "elev") && !is.null(dir_elev)) {
    raster_files <- list.files(path = dir_elev, pattern = "\\.tif$",
                               all.files = FALSE, full.names = TRUE)
    # What happens in case `which_clim == "elev" && is.null(dir_elev)`?

  } else {

    switch( # Why switch when all outputs are the same?
      c_source,
      "CHELSA" = {
        raster_files <- list.files(path = file_path, pattern = "\\.tif$",
                                   all.files = FALSE, full.names = TRUE)
      },
      "WorldClim" = {
        raster_files <- list.files(path = file_path, pattern = "\\.tif$",
                                   all.files = FALSE, full.names = TRUE)
      }
    )

  }

  ### raster stack
  cstack <- terra::rast(raster_files)

  # Only one band in elev, while the others are stacks and need naming
  if (!isTRUE(which_clim == "elev")) names(cstack) <- month.abb

  # set the switch
  expr <- paste(location_type, sd)

  switch(
    expr,
    "SpatialPointsDataFrame FALSE" = {

      if (!which_clim == "elev") {

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
  # Convert sf locations to SP
  if (inherits(location, c("sf", "sfc"))) {
    location <- sf::as_Spatial(sf::st_zm(location))
  }

  return(location)
}

.c_source_helper <- function(c_source) {
  # Check if the c_source argument is correct.
  # Using partial, case-insensitive matching.
  if (is.na(pmatch(toupper(c_source), c("CHELSA", "WORLDCLIM"))))
    stop("c_source must be either CHELSA, WorldClim") else {
      return(c_source)
    }
}

.var_helper <- function(var) {
  # Check if the var argument is correct.
  if (is.na(match(var, c("ALL", "prec", "tmax", "tmean", "tmin")))) {
    stop("var must be either ALL, prec, tmax, tmean, tmin")
  } else {
    return(var)
  }
}

.dir_helper <- function(var, dir_clim, dir_elev) {
  # Checks if the dir_clim and dir_elev arguments are correct.

  if(var == "ALL"){

    # Checks to make sure that all climate folders are there
    if (!all(c("prec", "tmax", "tmean", "tmin") %in% list.files(dir_clim))) {
      stop("dir_clim must contain prec, tmax, tmean, tmin subfolders")
    }

    if (!is.null(dir_elev) && !length(list.files(dir_elev)) >= 1 ) {
      stop("dir_elev is empty, or contains too many files")
    }

    # Checks that there are 12 rasters in each subfolder
    invisible(lapply(
      list.files(dir_clim),
      FUN = function(x){
        rast_nr <- length(list.files(paste0(dir_clim, "/", x)))
        rast_nr <- 12
        if(!rast_nr == 12) {
          stop(paste(x, "contains", rast_nr, "files out of 12"))
        }
      }
    ))

  }

  # Check each climate folder (if only one is supplied)
  if(sum(var %in% c("prec", "tmax", "tmean", "tmin")) > 0) {

    for(subfolder in seq_along(var)) {

      rast_nr <- length(list.files(paste0(dir_clim, "/", var[subfolder])))

      if(!rast_nr > 0) {
        stop(paste("Subfolder does not exist for", var[subfolder]))
      }

      if(!rast_nr == 12) {
        stop(paste(x, "contains", rast_nr, "files out of 12"))
      }

    }

  }

}

.location_helper <- function(location, location_g, location_df) {
  # Checks if the location argument is correct.

  # Assigns an 'id' if location_g is empty
  if (is.null(location_g)) {
    message(
      paste(
        "location_g must be one of",
        paste((colnames(location_df)[
          !colnames(location_df) %in% c("coords.x1", "coords.x2")
        ]), collapse = ", "), sep = ": "
      )
    )
    message("Defaulting to a unique id for each polygon or point object")
    location$Name <- location$id
    #location_g <- "id"

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
#' @template output_dir_clim_param
#' @template output_dir_elev_param
#' @template output_location_param
#' @template output_location_g_param
#' @template output_c_source_param
#' @template output_var_param
#'
#' @return
#' Returns a list storing matrices containing the mean and standard deviation
#' of the climate and/or elevation data. Each column represents a month, each
#' row represents a feature of the \code{location} \code{sp}, \code{sf} polygon
#' or point object. Values returned are either degrees Celsius for (tmax, tmean,
#' tmin) or mm (prec).
#'
#' @author James L. Tsakalos
#' @seealso The downloading ([`ce_download()`]), and the plotting
#' ([`plot_h()`] & [`plot_wl()`]) functions.
#'
#' @examples
#' \dontrun{
#'
#' # Extraction time will depend on the size of the polygon or point file.
#' # Import the Sibillini National Park Boundary
#' data("Sibillini_py")
#' # Run the download function
#' ce_extract(
#'   dir_clim = "../WorkingDirectory/CHELSA",
#'   dir_elev = "../WorkingDirectory/elev",
#'   location = Sibillini_py,
#'   location_g = "Position",
#'   c_source = "CHELSA"
#' )
#'
#' }
#' @importFrom exactextractr exact_extract
#' @importFrom glue glue
#' @importFrom sp SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom sf st_as_sf
#' @importFrom stats aggregate
#' @importFrom terra centroids crds extract rast vect
#' @importFrom dplyr %>% group_by summarize
#' @export
ce_extract <- function(
    dir_clim = NULL, dir_elev = NULL,
    location = NULL, location_g = NULL,
    c_source = "WorldClim", var = "ALL") {

  # Check if the properties of 'location' are correct.
  location <- .spat_helper(location)
  location@data$id <- seq_along(location) - 1
  location_df <- data.frame(location)

  # Check if the properties of var are correct
  var <- .var_helper(var)

  # Check if the directory paths are correct
  .dir_helper(dir_clim, dir_elev)

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
  if (var == "ALL") {
    files <- c("tmean", "tmin", "tmax", "prec", "elev")
  } else {
    files <- var
  }

  # If no directory provided for elev, no data to be extracted
  if (is.null(dir_elev)) {
    # Elev treated separately because it is not always used / required.
    # four options for var, here and elsewhere
    var <- var[var %in% c("tmean", "tmin", "tmax", "prec")]
    files <- files[files %in% c("tmean", "tmin", "tmax", "prec")]
  }

  result <- NULL
  result <- unlist(lapply(files, FUN = function(x) {
    .clim_extract(which_clim = x,
                  location = location,
                  location_type = location_type,
                  location_g = location_g,
                  sd = sd, result = result,
                  c_source = c_source,
                  dir_clim = dir_clim,
                  dir_elev = dir_elev)
  }), recursive = FALSE)

  # If we need to adjust the values
  if (c_source == "CHELSA") {
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

      if (colnames(lat)[1] == "location_g") {
        lat <- aggregate(lat ~ location_g, data = lat, mean)
      }

      result[["lat"]] <- lat

    },
    "SpatialPolygonsDataFrame" = {

      if (!location_g == "id") {

        lat <- data.frame(
          "location_g" = location$Name,
          "lat" = round(
            terra::crds(terra::centroids(terra::vect(location)))[, 2], 3)
        )

        ifelse(location_g == "id", # Strange use for ifelse; why not if()?
               {colnames(lat)[1] <- "id"},
               {colnames(lat)[1] <- "location_g"})
        result[["lat"]] <- lat

      } else {

        lat <- data.frame(
          "location_g" = location[[location_g]],
          "lat" = round(
            terra::crds(terra::centroids(terra::vect(location)))[, 2], 3
          )
        )

        ifelse(location_g == "id",
               {colnames(lat)[1] <- "id"},
               {colnames(lat)[1] <- "location_g"})
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
