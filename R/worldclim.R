.download_dir <- function(clim_points, var, output_dir, ...) {

  # Helper function to download and mosaic the tiles

  # Make an empty list to fill
  clim_list <- list()

  # lats
  lats <- clim_points[, "y"]

  # Downloads the tiles and stores into that list
  for (pts in 1:seq_along(lats)) {

    tile <- geodata::worldclim_tile(
      var,
      res = 0.5,
      lon = clim_points[pts, "x"], lat = clim_points[pts, "y"],
      path = tempfile(),
      version = "2.1",
      mode = mode,

      ...
    )

    clim_list[[pts]] <- tile

  }

  # Mosaic the tiles in the list
  if (length(seq_along(lats)) > 1) {
    clim_list$fun <- mean
    clim_mosaic <- do.call(terra::mosaic, clim_list)
  } else {
    clim_mosaic <- clim_list[[1]]
  }

  # Name the bands (at this stage, multiband raster)
  names(clim_mosaic) <- paste(
    "wc2.1_30s", var, sprintf("%02d", 1:12), sep = "_"
  )

  # Export the climate mosaic
  lapply(c(1:12), FUN = function(x){
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
#' `worldclim()` downloads the WorldClim V2.1 climate data for 1970-2000. This
#' includes monthly climate data for minimum, mean, and maximum temperature and
#' precipitation at a resolution of 0.5 minutes of a degree.
#'
#' @template output_dir_param
#' @template output_location_param
#' @param var,mode,\dots Arguments to control a download from the Internet
#' `download.file()`.
#'
#' @return
#' Creates four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year for the time
#' period 1970&ndash;2000. Each of the files are downloaded at a spatial
#' resolution of 0.5 minutes of a degree. The precipitation folder contains
#' average monthly precipitation (mm). The tmax folder contains maximum monthly
#' temperature. The tmin folder contains minimum monthly temperature. The tmean
#' folder contains the average monthly temperature. The unit of measure for
#' temperature is in &deg;C. This function uses the \pkg{geodata} to download
#' the worldclim tiles.
#'
#' @author James L. Tsakalos
#' @seealso Downloading from CHELSA [`chelsa()`] or a more convenient
#' function for downloading other climate and elevation data [`ce_download()`].
#' @references{ Fick, S.E. and R.J. Hijmans. (2017). WorldClim 2: new 1km
#' spatial resolution climate surfaces for global land areas. International
#' Journal of Climatology. 37, 4302–4315.
#' }
#'
#' @examples
#' \dontrun{
#' # Download time will depend on the size of the area you wish to access
#' # climate data for and your internet connection speed.
#'
#' # Import the Italian Biome data set
#' data("it_py", package = "climenv")
#'
#' # Download the WorldClim data
#' worldclim(
#'   output_dir = "...Desktop/worldclim",
#'   location = "it_py"
#' )
#'
#' }
#' @importFrom utils data download.file unzip
#' @importFrom terra rast extract xyFromCell mosaic writeRaster vect
#' @importFrom geodata worldclim_tile
#' @export
worldclim <- function(output_dir, location, mode = "wb",
                      var = "all", ...) {

  # Check the var argument
  stopifnot(var %in% c("all", "prec", "tmax", "tmin", "tavg"))

  # If var is NULL, all climate variables can be downloaded.
  if (var == "all") {
    var <- c("prec", "tmax", "tmin", "tavg")
  }

  # create WorldClim tiles
  rs <- terra::rast(nrows = 5, ncols = 12,
                    xmin = -180, xmax = 180,
                    ymin = -60, ymax = 90)
  rs[] <- 1:60

  # Intersect location and tiles
  tiles <- unique(terra::extract(rs, terra::vect(location))$lyr.1)
  clim_points <- terra::xyFromCell(rs, which(rs[] %in% tiles))

  # Make the folders
  for (path in var) {
    if (!dir.exists(paste0(output_dir, "/", path)))
      dir.create(paste0(output_dir, "/", path),
                 recursive = TRUE, showWarnings = FALSE)
  }

  # This runs through every variable which is supplied
  invisible(
    lapply(var, FUN = function(x) {
      .download_dir(clim_points, var = x, output_dir, ...)
    })
  ) # The invisible part stops lapply from printing to console

}
