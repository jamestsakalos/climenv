# Helper function to download and mosaic the tiles
.download_dir <- function(clim_points, var, output_dir, ...) {
  # latitudes
  lats <- clim_points[, "y"]
  temp_files <- vapply(paste0("temp-tile-", seq_along(lats)),
                       tempfile, character(1), tmpdir = output_dir)
  # Remove temporary downloads when function exits
  on.exit(lapply(temp_files, unlink))

  # Downloads the tiles and stores into that list
  clim_list <- lapply(seq_along(lats), function(pts) {
    geodata::worldclim_tile(
      var,
      res = 0.5,
      lon = clim_points[pts, "x"], lat = clim_points[pts, "y"],
      path = temp_files[pts],
      version = "2.1",
      ...
    )
  })

  # Mosaic the tiles in the list
  if (length(lats) > 1) {
    clim_list$fun <- mean
    clim_mosaic <- do.call(terra::mosaic, clim_list)
  } else if (length(clim_list) == 0) {
    stop("No data downloaded") # nocov
  } else {
    clim_mosaic <- clim_list[[1]]
  }

  # Name the bands (at this stage, multiband raster)
  names(clim_mosaic) <- paste(
    "wc2.1_30s", var,
    # climate data length 12; elev, 1
    sprintf("%02d", seq_along(names(clim_mosaic))),
    sep = "_"
  )

  message("Writing to: ", output_dir)
  # Export the climate mosaic
  lapply(seq_along(names(clim_mosaic)), FUN = function(x) {
    message("  File: ",
            paste0(output_dir, "/", var, "/", names(clim_mosaic)[x], ".tif"))
    terra::writeRaster(
      clim_mosaic[[x]],
      paste0(output_dir, "/", var, "/", names(clim_mosaic)[x], ".tif"),
      overwrite = TRUE
    )
  })
}

#' Download WorldClim climate data
#'
#' @description
#' `worldclim()` downloads the WorldClim V2.1 climate data for 1970&ndash;2000.
#' This includes monthly climate data for minimum, mean, and maximum temperature
#' and precipitation at a resolution of 0.5 minutes of a degree.
#' This function uses the \pkg{geodata} to download the worldclim tiles.
#'
#' @template output_dir_param
#' @template output_location_param
#' @param var,\dots Arguments to control a download from the Internet
#' `download.file()`.
#'
#' @return
#' `worldclim()` is called for its side effects and returns `NULL`.
#' Creates four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year for the time
#' period 1970&ndash;2000. Each of the files are downloaded at a spatial
#' resolution of 0.5 minutes of a degree. The precipitation folder contains
#' average monthly precipitation (mm). The tmax folder contains maximum monthly
#' temperature. The tmin folder contains minimum monthly
#' temperature. The tmean folder contains the average monthly temperature.
#' Temperature values are reported in &deg;C.
#'
#' @author James L. Tsakalos
#' @seealso Downloading from CHELSA [`chelsa()`] or a more convenient
#' function for downloading other climate and elevation data [`ce_download()`].
#' @references{ Fick, S.E. & Hijmans, R.J. (2017). WorldClim 2: new 1km
#' spatial resolution climate surfaces for global land areas. International
#' Journal of Climatology. 37, 4302&ndash;4315. \doi{10.1002/joc.5086}
#'
#' }
#' @examples
#' \dontrun{
#' # Download time will depend on the size of the area you wish to access
#' # climate data for and your internet connection speed.
#'
#' # Lets make a polygon file
#' regents <- sf::st_polygon(
#'   list(
#'     cbind(
#'       "lon" = c(51.537, 51.525, 51.523, 51.530, 51.534, 51.537),
#'       "lat" = c(-0.150, -0.145, -0.156, -0.167, -0.163, -0.150)
#'     )
#'   )
#' )
#'
#' # Download the WorldClim data
#' worldclim(
#'   output_dir = "...Desktop/worldclim",
#'   location = "regents"
#' )
#'
#' }
#' @importFrom utils data download.file unzip
#' @importFrom terra rast extract xyFromCell mosaic writeRaster vect
#' @importFrom geodata worldclim_tile
#' @export
worldclim <- function(output_dir, location, var = "all",
                      ...) {

  var_options <- c("prec", "tmax", "tmin", "tavg", "elev")

  # If var is NULL, all climate variables can be downloaded.
  if (is.null(var) || "all" %in% var) {
    var <- c("prec", "tmax", "tmin", "tavg")
  }

  if (any(!var %in% var_options)) {
    stop("invalid `var`; select from ", paste(var_options, collapse = ", "))
  }

  # create WorldClim tiles
  rs <- terra::rast(nrows = 6, ncols = 12,
                    xmin = -180, xmax = 180,
                    ymin = -90, ymax = 90)
  rs[] <- 1:72

  # Intersect location and tiles
  tiles <- unique(terra::extract(rs, terra::vect(location))$lyr.1)
  if (any(is.na(tiles))) {
    warning("Could not map all coordinates to tiles; check validity")
  }
  clim_points <- terra::xyFromCell(rs, which(rs[] %in% tiles))
  if (length(clim_points) == 0) {
    return(NULL)
  }

  # Make the folders
  for (path in var) {
    if (!dir.exists(paste0(output_dir, "/", path)))
      dir.create(paste0(output_dir, "/", path),
                 recursive = TRUE, showWarnings = FALSE)
  }

  # This runs through every variable which is supplied
  lapply(var, FUN = function(x) {
    .download_dir(clim_points, var = x, output_dir, ...)
  })

  # Return:
  invisible()
}
