#' @importFrom terra colFromCell crs rowFromCell mask
.elev_geodata <- function(location, output_dir, ...) {

  # create SRTM tiles
  y_max <- 60
  y_min <- -60
  rs <- terra::rast(res = 5, ymin = y_min, ymax = y_max,
                    vals = 1:1728, crs = "+proj=longlat +datum=WGS84")
  # mask out the tiles with no data
  rs <- terra::mask(rs, terra::vect(climenv::srtm_tiles), touches = TRUE)

  # intersect location and tiles
  tiles <- unique(
    terra::extract(rs, location, touches = TRUE)$lyr.1
  )
  if (all(is.na(tiles))) {
    return(NULL)
  }
  cols <- formatC(terra::colFromCell(rs, tiles), width = 2, flag = 0)
  rows <- formatC(terra::rowFromCell(rs, tiles), width = 2, flag = 0)
  na <- cols == "NA" | rows == "NA"
  srtm_id <- paste0("srtm_", cols[!na], "_", rows[!na])

  srtm_list <- lapply(srtm_id, function(id) {

    temp_file <- tempfile("srtm_", output_dir)
    on.exit(unlink(temp_file))

    tif <- paste0(output_dir, "/", id, ".tif")
    error <- if (file.exists(tif)) {
      0
    } else {
      zip_url <- paste0(
        "https://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/",
        id, ".zip"
      )
      url_status <- attr(curlGetHeaders(zip_url, verify = FALSE), "status")
      error <- if (url_status == 200) {
        if (tryCatch(
          utils::download.file(url = zip_url, destfile = temp_file, mode = "wb",
                               ...), # Returns 0 on success
          error = function(e) {
            warning("Failed to download ", id, ": ", e)
            -1 # Error code
          }
        ) == 0) {
          if (isFALSE(tryCatch(
            utils::unzip(temp_file, paste0(id, ".tif"), exdir = output_dir),
            error = function(e) {
              warning("Temporary file not found: ", temp_file)
              FALSE
            }
          ))) {
            -2 # Error code
          } else {
            0 # Success code
          }
        }
      } else {
        warning("Could not download ", id, ": HTTP status ", url_status)
        -1
      }
    }
    if (error == 0) {
      rs <- terra::rast(tif)
      terra::crs(rs) <- "+proj=longlat +datum=WGS84"
      rs
    } else {
      NULL
    }
  })

  srtm_list <- srtm_list[!vapply(srtm_list, is.null, logical(1))]

  # Mosaic the tiles in the list
  if (length(srtm_list) > 1) {
    srtm_list$fun <- mean
    srtm_mosaic <- do.call(terra::mosaic, srtm_list)
  } else if (length(srtm_list) == 0) {
    stop("No data downloaded.")
  } else {
    srtm_mosaic <- srtm_list[[1]]
  }

  return(srtm_mosaic)
}

#' Download elevation data
#'
#' @description
#' `elev()` downloads elevation data the Shuttle Radar Topography Mission
#' (SRTM), specifically the hole-filled CGIAR-SRTM (90 m resolution) for
#' latitudes between -60 and 60 or Mapzen's synthesis digital elevation product.
#'
#' @template output_dir_param
#' @template output_location_param
#' @template output_e_source_param
#' @param verbose Logical specifying whether to display verbose output when
#' downloading from Mapzen.
#' @param \dots Additional arguments to [`download.file()`].
#'
#' @returns
#' `elev()` is called for its side-effects.
#' It invisibly returns a "SpatRaster" object if files were downloaded
#' successfully, and returns `NULL` otherwise. If the elevation data is sourced
#' from geodata the SpatRaster is downloaded at a spatial resolution of 30 arc
#' seconds (&#126;1 km  sq.). If elevation data is from Mapzen then the
#' SpatRaster will be a mosaic. Specifically, Mapzen's SpatRaster is unique as
#' it combines several sources of digital elevation models, including SRTM, the
#' ArcticDEM (covering all areas north of 60&#176;), EUDEM (digital elevation
#' model over Europe; for review, see Mouratidis & Ampatzidis, 2019), and others
#' into a single product. The resolution of this SpatRaster was set to 7,
#' corresponding to 611.5 m ground resolution at 60&#176; latitude 864.8 m at
#' 45&#176; and 1223 m at 0&#176;.
#'
#' @author James L. Tsakalos and Martin R. Smith
#' @seealso A more convenient function for other climate and elevation data
#' [`ce_download()`]. See [sf::st_polygon] to make polygons and [sf::st_as_sf]
#' to make point objects.
#' @references{ Hijmans, R.J., Barbosa, M., Ghosh, A., & Mandel, A. (2023).
#' geodata: Download Geographic Data. R package version 0.5-8.
#' \url{https://CRAN.R-project.org/package=geodata}
#'
#' Hollister, J. (2022). elevatr: Access Elevation Data from Various
#' APIs. R package version 1.0.0. \doi{10.5281/zenodo.5809645}
#' \url{https://CRAN.R-project.org/package=elevatr}
#'
#'
#' Mouratidis, A., & Ampatzidis, D. (2019). European Digital Elevation Model
#' Validation against Extensive Global Navigation Satellite Systems Data and
#' Comparison with SRTM DEM and ASTER GDEM in Central Macedonia (Greece).
#' ISPRS International Journal of Geo-Information 8, 108.
#' \doi{10.3390/ijgi8030108}
#' }
#'
#' @examples
#' \donttest{
#' # We could do this using Italy's Biome data
#' data("italy_py", package = "climenv")
#'
#' # Create temporary file
#' temp_path <- tempfile()
#' on.exit(unlink(file.path(temp_path)), add = TRUE)
#'
#' # elevation will be saved in the output_dir (i.e. output directory)
#'    elev(output_dir = temp_path, location = italy_py)
#' }
#'
#' # As a smaller example, we can make a polygon covering an ocean island.
#' location <- sf::st_polygon(
#'    list(
#'      cbind(
#'        long = c(161, 161, 154, 161),
#'        lat = c(-61, -49, -61, -61)
#'      )
#'    )
#' )
#'
#' # We need to make sure that the polygon the correct class
#' location <- sf::st_geometry(location)
#' class(location) # "sfc_POLYGON" "sfc"
#'
#' # Set the coordinate reference system
#' sf::st_crs(location) = "epsg:4326"
#'
#' # We are now ready to call elev()
#' # elev(location = location, output_dir = temp_path)
#'
#' @importFrom elevatr get_elev_raster
#' @importFrom methods as
#' @importFrom sf as_Spatial st_as_sf st_bbox st_geometry st_is_longlat st_crs<-
#' @importFrom terra extract mosaic rast rasterize vect writeRaster xyFromCell
#' @export
elev <- function(output_dir, location, e_source = "mapzen",
                 verbose = FALSE, ...) {

  e_source_id <- pmatch(tolower(e_source[1]), c("mapzen", "geodata"))
  if (is.na(e_source_id)) {
    stop("e_source must be \"mapzen\" or \"geodata\"")
  }

  if (is.function(location)) {
    location <- st_as_sf(location)
  }
  if (inherits(location, c("sfc", "sfg", "SpatVector"))) {
    location <- as(location, "Spatial")
  }
  location_sf <- as(location, "sf")

  # Check that the bounding box coordinates
  bbox <- sf::st_bbox(location_sf)
  if (bbox[["xmin"]] < -180 || bbox[["xmax"]] > 180) {
    stop("`location` bounding box falls outside supported longitudes ",
         "-180 to 180")
  }
  if (bbox[["ymin"]] < -90 || bbox[["ymax"]] > 90) {
    stop("`location` bounding box falls outside supported latitudes ",
         "-90 to 90")
  }

  if (!isTRUE(sf::st_is_longlat(location_sf))) {
    warning("Coordinate reference system not specified; assuming WGS84")
    sf::st_crs(location_sf) <- "+proj=longlat +datum=WGS84"
  }

  # Create elev folder
  if (!dir.exists(paste0(output_dir, "/elev"))) {
    dir.create(paste0(output_dir, "/elev"),
               recursive = TRUE, showWarnings = FALSE)
  }

  file_path <- paste0(output_dir, "/elev/srtm.tif")
  # Saves elevation from geodata or mapzen sources
  switch(e_source_id, { # mapzen
    elev_raster <- elevatr::get_elev_raster(
      location_sf, z = 7, override_size_check = TRUE,
      progress = verbose, verbose = verbose
    )
    srtm_mosaic <- as(elev_raster, "SpatRaster")
    terra::writeRaster(srtm_mosaic, filename = file_path, overwrite = TRUE)
  }, { # geodata

    srtm_mosaic <- .elev_geodata(location_sf, output_dir, ...)
    if (is.null(srtm_mosaic)) {
      NULL
    } else {
      terra::writeRaster(srtm_mosaic, filename = file_path, overwrite = TRUE)
    }
  }
  )
}
