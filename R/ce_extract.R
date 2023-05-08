.clim_extract <- function(which_clim, location,
                          location_type, location_g,
                          sd, temp, c_source, dir_clim, dir_elev) {


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
        temp[[glue::glue("{which_clim}")]] <- extract_m

      } else {

        # For elevation, no mean required, no grouping variable
        extract_m <- terra::extract(cstack, terra::vect(location))
        extract_m$location_g <- as.factor(location[[location_g]])

        dat <- extract_m[, c(3, 2)]
        names(dat) <- c("id", "mean")
        temp[[glue::glue("{which_clim}")]] <- dat

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

          temp[["abmt"]] <- extract_abmt

        }

        extract_m <- aggregate(. ~ location_g, extract_m,
                               FUN = function(x) {
                                 mean(x, na.rm = TRUE)
                               })

        temp[[glue::glue("{which_clim}_m")]] <- extract_m
        temp[[glue::glue("{which_clim}_sd")]] <- extract_sd

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

        temp[[glue::glue("{which_clim}")]] <- data.frame(extract_m)

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
        temp[[glue::glue("{which_clim}_m")]] <- extract_m

        if (which_clim == "tmin") {

          extract_m <- exactextractr::exact_extract(
            cstack, location, "min", progress = FALSE
          )
          colnames(extract_m) <- month.abb
          extract_abmt <- cbind(
            data.frame(id = 0:c(dim(extract_m)[1] - 1)), extract_m
          )
          temp[["abmt"]] <- extract_abmt

        }

        extract_m <- exactextractr::exact_extract(
          cstack, location, "stdev", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(
          data.frame(id = 0:c(dim(extract_m)[1] - 1)), extract_m
        )
        temp[[glue::glue("{which_clim}_sd")]] <- extract_m

      } else {

        # For elevation, zonal for polygons, no grouping variable
        extract_m <- exactextractr::exact_extract(
          cstack, location, c("mean", "stdev"), progress = FALSE
        )

        extract_m <- cbind(
          data.frame(id = as.factor(location[[location_g]]), extract_m)
        )

        temp[[glue::glue("{which_clim}")]] <- extract_m

      }
    },
    "SpatialPolygonsDataFrame TRUE" = {

      if (!which_clim == "elev") {

        extract_m <- exactextractr::exact_extract(
          cstack, location, "mean", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        temp[[glue::glue("{which_clim}_m")]] <- extract_m

        if (which_clim == "tmin") {

          extract_m <- exactextractr::exact_extract(
            cstack, location, "min", progress = FALSE
          )
          colnames(extract_m) <- month.abb
          extract_abmt <- cbind(
            data.frame(location_g = location$Name), extract_m
          )
          temp[["abmt"]] <- extract_abmt

        }

        extract_m <- exactextractr::exact_extract(
          cstack, location, "stdev", progress = FALSE
        )
        colnames(extract_m) <- month.abb
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        temp[[glue::glue("{which_clim}_sd")]] <- extract_m

      } else {

        extract_m <- exactextractr::exact_extract(
          cstack, location, c("mean", "stdev"), progress = FALSE
        )
        extract_m <- cbind(data.frame(location_g = location$Name), extract_m)
        temp[[glue::glue("{which_clim}")]] <- extract_m

      }
    }
  )

  return(temp)

}

#' ce_extract
#'
#' @description
#' \code{ce_extract} brings together the \code{worldclim},
#' \code{chelsa} and \code{elev} functions into one location to streamline data
#' downloading.
#'
#' @param dir_clim Character. Directory containing the climate data. Must
#' feature prec, tmax, tmean and tmin subfolders.
#' @param dir_elev Character. Directory containing the elevation data.
# Suggest linking to definition of these classes so user can find out how
# to format data as required.
#' @param location A \code{sp}, \code{sf} polygon or point object.
#' @param location_g Character. Informs how the zonal statistics are exported.
#' Should correspond to a column of the \code{location} argument. If NULL,
#' the zonal statistics are calculated for all features of \code{location}.
#' @param c_source Character. Supply either \code{"CHELSA"} or
#' \code{"WorldClim"} argument.
#' If `NULL`, then ((what happens?)).
#' @param var Character. If supplied will export a subset ((which?)) of the climate or
#' elevation data.
#'
#' @return
#' Returns a list storing matrices containing the mean and standard deviation
#' of the climate and/or elevation data. Each column represents a month, each
#' row represents a feature of the \code{location} \code{sp}, \code{sf} polygon
#' or point object. Values returned are either degrees Celsius for (tmax, tmean,
#' tmin) or mm (prec).
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
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
#'   location_g = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
#'   c_source = "CHELSA"
#' )
#'
#' }
#' @seealso PLOTTTTTTTIN XXXXX functions.
#' @importFrom exactextractr exact_extract
#' @importFrom glue glue
#' @importFrom sp SpatialPointsDataFrame SpatialPolygonsDataFrame
#' @importFrom stats aggregate
#' @importFrom terra centroids crds extract rast vect
#' @importFrom dplyr %>% group_by summarize
#' @export
ce_extract <- function(
    dir_clim = NULL, dir_elev = NULL,
    location = NULL, location_g = NULL,
    c_source = NULL, var = "ALL") {

  # Convert sf locations to SP
  if (inherits(location, c("sf", "sfc"))) {
    location <- sf::as_Spatial(sf::st_zm(location))
  }

  # Why not:
  location@data$id <- seq_along(location) - 1
  location_df <- data.frame(location)

  # Check if the c_source argument is correct
  # See comments in ce_download
  if (is.na(match(c_source, c("NULL", "CHELSA", "WorldClim"))))
    stop("c_source must be either NULL, CHELSA, WorldClim")

  # Check if the var argument is correct
  if (is.na(match(var, c("ALL", "prec", "tmax", "tmean", "tmin"))))
    stop("c_source must be either ALL, prec, tmax, tmean, tmin")

  # Check if the directory paths are correct
  if (var == "ALL" || var == "elev") {

    if (!all(c("prec", "tmax", "tmean", "tmin") %in% list.files(dir_clim))) {
     stop("dir_clim must contain prec, tmax, tmean, tmin subfolders")
    }

    if (!is.null(dir_elev)) {
      if (length(list.files(dir_elev)) != 1) {
        stop("dir_elev is empty")
      }
  }

  # Proposed replacement for the below code:
  if (!dir.exists(paste0(dir_clim, "/", var))) {
    stop("Subfolder '", var, "' not found in directory ", dir_clim)
  }
  # switch(
  #   var,
  #   "prec" = {
  #     # Why not use this simpler formulation?
  #     # if (sum(c("prec") %in% list.files(dir_clim)) != 1)
  #     # or better still
  #     # if (!("prec" %in% list.files(dir_clim)))
  #     # or, I think the intention:
  #     if (!dir.exists(paste0(dir_clim, "/", prec)))
  #       stop("dir_clim must contain prec subfolder")
  #   },
  #   "tmax" = {
  #     if (isFALSE(
  #       sum(c("tmax") %in% list.files(dir_clim)) == 1
  #     )) stop("dir_clim must contain tmax subfolder")
  #   },
  #   "tmean" = {
  #     if (isFALSE(
  #       sum(c("tmean") %in% list.files(dir_clim)) == 1
  #     )) stop("dir_clim must contain tmean subfolder")
  #   },
  #   "tmin" = {
  #     if (isFALSE(
  #       sum(c("tmin") %in% list.files(dir_clim)) == 1
  #     )) stop("dir_clim must contain tmin subfolder")
  #   }
  # )

  # Format the location file
  if (is.null(location_g)) {
    message(
      paste(
        "location_g must be one of",
        paste((colnames(location_df)[
          !colnames(location_df) %in% c("coords.x1", "coords.x2")
        ]), collapse = ", "), sep = ": "
      )
    )
    message("Defaulting to id") # MS: will a user know what this means?
    location$Name <- location$id
    location_g <- "id"
    sd <- FALSE
  } else {
    # clearer if explicitly specified that this is an "else"
    sd <- TRUE # if no 'grouping' factor doesn't make sense to calculate sd.
  }

  filter <- colnames(location_df)[
    !colnames(location_df) %in% c("coords.x1", "coords.x2", "optional")
  ]

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
        st_as_sf(location) %>%
        group_by("Name" = Name)
    },
    "SpatialPointsDataFrame TRUE" = {
      location <- location %>%
        st_as_sf(location) %>%
        group_by("Name" = Name)
    },
    "SpatialPolygonsDataFrame TRUE" = {
      location <- location %>%
        st_as_sf(location) %>%
        group_by("Name" = Name) %>%
        summarize()
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
    # MS: I'm not quite clear why elev is treated separately from the other
    # four options for var, here and elsewhere
    var <- var[var %in% c("tmean", "tmin", "tmax", "prec")]
    files <- files[files %in% c("tmean", "tmin", "tmax", "prec")]
  }

  temp <- NULL
  for (vars in files) {
    # This requires copying temp `length(vars)` times: memory-hungry & slow!
    # Better if possible to remove `temp` argument and do something like:
    # temp <- unlist(lapply(vars, .clim_extract), recursive = FALSE)
    temp <- .clim_extract(which_clim = vars,
                          location = location,
                          location_type = location_type,
                          location_g = location_g,
                          sd = sd, temp = temp,
                          c_source = c_source,
                          dir_clim = dir_clim,
                          dir_elev = dir_elev)
  }

  # If we need to adjust the values
  if (c_source == "CHELSA") {
    toconvert <- names(temp)[grepl("t", names(temp))]
    for (i in toconvert) {
      temp[[i]][, 2:13] <- (temp[[i]][, 2:13] / 1)
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

      if(colnames(lat)[1] == "location_g") {
        lat <- aggregate(lat~location_g, data = lat, mean)
      }

      temp[["lat"]] <- lat

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
        temp[["lat"]] <- lat

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
        temp[["lat"]] <- lat

      }

    }
  )
  # temp is a misleading name if this is what's being returned!
  # consider `result` instead?
  return(temp)
}
