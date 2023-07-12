#' @importFrom terra colFromCell crs<- rowFromCell
.elev_geodata <- function(location, output_dir, ...) {

  y_max <- 60
  y_min <- -60
  # create SRTM tiles
  rs <- terra::rast(res = 5, ymin = y_min, ymax = y_max)
  rs <- terra::rasterize(terra::vect(climenv::srtm_tiles), rs, "FID")

  # we need to set the crs of the SRTM tiles
  terra::crs(rs) <- "epsg:4326"

  # the location crs needs to match the tiles
  location <- terra::project(terra::vect(location), rs)

  # Intersect location and tiles
  tiles <- unique(
    terra::extract(rs, location, touches = TRUE)$FID
  )
  cols <- formatC(colFromCell(rs, tiles), width = 2, flag = 0)
  rows <- formatC(rowFromCell(rs, tiles), width = 2, flag = 0)
  na <- cols == "NA" | rows == "NA"
  srtm_id <- paste0("srtm_", cols[!na], "_", rows[!na])

  temp_file <- tempfile("srtm_", output_dir)
  on.exit(unlink(temp_file))

  srtm_list <- lapply(srtm_id, function(id) {
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
        tryCatch(
          utils::download.file(url = zip_url, destfile = temp_file, mode = "wb",
                               ...), # Returns 0 on success
          error = function(e) {
            warning("Failed to download ", id, ": ", e)
            -1 # Error code
          }
        )
      } else {
        warning("Could not download ", id, ": HTTP status ", url_status)
        -1
      }
    }
    if (error == 0) {
      tryCatch(utils::unzip(temp_file, paste0(id, ".tif"), exdir = output_dir),
               error = function(e) warning("Failed to unzip: ", id))
      rs <- rast(tif)
      crs(rs) <- "+proj=longlat +datum=WGS84"
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
#' @param \dots Additional arguments to [`download.file()`].
#'
#' @return
#' Creates one subfolder named elev storing a raster (.tiff). If elevation is
#' sourced from geodata the elevation is downloaded at a spatial resolution of
#' 30 arc seconds (~1 km  sq.). If elevation data is from mapzen then the
#' product will be a mosaic. Specifically, Mapzen’s product is unique as it
#' combines several sources of digital elevation models, including SRTM, the
#' ArcticDEM (covering all areas north of 60°), EUDEM (digital elevation model
#' over Europe; for review, see Mouratidis & Ampatzidis, 2019), and others into
#' a single product. The resolution of this product was set to 7, corresponding
#' to 611.5 m ground resolution at 60° latitude 864.8 m at 45° and 1223 m at 0°.
#'
#' @author James L. Tsakalos
#' @seealso A more convenient function for other climate and elevation data
#' [`ce_download()`]. See [sf::st_polygon] to make polygons and [sf::st_as_sf]
#' to make point objects.
#' @references{ Hijmans, R.J., Barbosa, M., Ghosh, A., & Mandel, A. (2023).
#' geodata: Download Geographic Data. R package version 0.5-8.
#' https://CRAN.R-project.org/package=geodata
#'
#' Hollister, J. (2022). elevatr: Access Elevation Data from Various
#' APIs. R package version 0.4.2. \doi{10.5281/zenodo.5809645}
#' https://CRAN.R-project.org/package=elevatr
#'
#' Mouratidis, A., & Ampatzidis, D. (2019). European Digital Elevation Model
#' Validation against Extensive Global Navigation Satellite Systems Data and
#' Comparison with SRTM DEM and ASTER GDEM in Central Macedonia (Greece).
#' ISPRS International Journal of Geo-Information 8, 108.
#' \doi{10.3390/ijgi8030108}
#' }
#'
#' @examples
#' \dontrun{
#' # We could do this using Italy's Biome data
#' data("italy_py", package = "climenv")
#' # elevation will be saved in the output_dir (i.e. output directory)
#'    elev(output_dir = "...Desktop/elev", location = italy_py)
#' }
#'
#' Or a smaller example we can make a polygon covering an island in the ocean.
#'
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
#' # Lets make sure to set the coordinate reference system
#' sf::st_crs(location) = "epsg:4326"
#'
#' # Lastly we can use elev()
#' elev_data <- elev(location = location)
#'
#' @importFrom elevatr get_elev_raster
#' @importFrom methods as
#' @importFrom sf as_Spatial st_geometry st_bbox st_is_longlat st_crs<-
#' @importFrom terra extract mosaic rast rasterize vect writeRaster xyFromCell
#' @export
elev <- function(output_dir, location, e_source = "mapzen", ...) {

  e_source_id <- pmatch(tolower(e_source[1]), c("mapzen", "geodata"))
  if (is.na(e_source_id)) {
    stop("e_source must be \"mapzen\" or \"geodata\"")
  }

  if (any(c("sfc", "sfg") %in% class(location))) {
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
    warning("Coordinate reference system not specified; assuming EPSG:4326")
    # TODO JS to check: Should we prefer WGS84 to match output?
    st_crs(location_sf) <- "EPSG:4326"
  }

  # Create elev folder
  if (!dir.exists(paste0(output_dir, "/elev"))) {
    dir.create(paste0(output_dir, "/elev"),
               recursive = TRUE, showWarnings = FALSE)
  }

  # Saves elevation from geodata or mapzen sources
  switch(e_source_id,
         { # mapzen
           srtm_mosaic <- terra::rast(
             elevatr::get_elev_raster(
               location_sf, z = 7, override_size_check = TRUE,
               progress = FALSE
             )
           )
           file_path <- paste0(output_dir, "/elev/srtm.tif")
           terra::writeRaster(srtm_mosaic, filename = file_path,
                              overwrite = TRUE)
         },
         { # geodata
           srtm_mosaic <- .elev_geodata(location_sf, output_dir, ...)
           file_path <- paste0(output_dir, "/elev/srtm.tif")
           terra::writeRaster(srtm_mosaic, filename = file_path,
                              overwrite = TRUE)
         }
  )
}
