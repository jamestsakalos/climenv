.elev_geodata <- function(location, output_dir) {

  # create SRTM tiles
  rs <- terra::rast(nrows = 24, ncols = 72,
                    xmin = -180, xmax = 180,
                    ymin = -60, ymax = 60)
  rs <- terra::rasterize(terra::vect(climenv::srtm_tiles), rs, "FID")

  # we need to set the crs of the SRTM tiles
  terra::crs(rs) <- "epsg:4326"

  # the location crs needs to match the tiles
  location <- terra::project(terra::vect(location), rs)

  # Intersect location and tiles
  tiles <- unique(
    terra::extract(rs, location, touches = TRUE)$FID
  )

  # if a polygon falls in an area where srtm is not covering
  srtm_points <- tiles[!is.na(tiles)]
  if (length(srtm_points) == 0) {
    stop("goedata could not map location to tiles")
  }

  # now to extract the points and the coordinates
  srtm_points <- terra::xyFromCell(rs, srtm_points)

  # Make an empty list to fill
  srtm_list <- list()

  # lats
  lats <- srtm_points[, "y"]

  # Downloads the tiles and stores into that list
  for (pts in seq_along(lats)) {

    tile <- geodata::elevation_3s(
      lon = srtm_points[pts, "x"], lat = srtm_points[pts, "y"],
      path = tempfile()
    )

    srtm_list[[pts]] <- tile

  }

  # Mosaic the tiles in the list
  if (length(lats) > 1) {
    srtm_list$fun <- mean
    srtm_mosaic <- do.call(terra::mosaic, srtm_list)
  } else {
    srtm_mosaic <- srtm_list[[1]]
  }
  return(srtm_mosaic)
}
#' Download elevation data
#'
#' @description
#' `elev()` downloads elevation data the Shuttle Radar Topography Mission
#' (SRTM) , specifically the hole-filled CGIAR-SRTM (90 m resolution) for
#' latitudes between -60 and 60 or Mapzen's synthesis digital elevation product.
#'
#' @template output_dir_param
#' @template output_location_param
#' @template output_e_source_param
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
#' @importFrom geodata elevation_3s
#' @importFrom sf as_Spatial st_geometry st_bbox st_is_longlat
#' @importFrom terra rast extract xyFromCell mosaic writeRaster rasterize vect
#' @export
elev <- function(output_dir, location, e_source = "mapzen") {

  e_source_id <- pmatch(tolower(e_source[1]), c("mapzen", "geodata"))
  if (is.na(e_source_id)) {
    stop("e_source must be \"mapzen\" or \"geodata\"")
  }

  # Convert to "sfc_POLYGON" "sfc"
  if ("sfg" %in% class(location)) {
    location <- sf::st_geometry(location)
  }

  # Convert sf locations to SP
  if (("sf" %in% class(location)) || ("sfc" %in% class(location))) {
    location <- sf::as_Spatial(location)
  }

  # Check that the bounding box coordinates
  if (!sum(
    sf::st_bbox(location)[c(1)] >= -180,
    sf::st_bbox(location)[c(2)] >= -90,
    sf::st_bbox(location)[c(3)] <= 180,
    sf::st_bbox(location)[c(4)] <= 90
  ) == 4) stop(
    "bounding box of location has potentially an invalid value range"
  )

  if (is.na(sf::st_is_longlat(location)) ||
      !sf::st_is_longlat(location) == TRUE) stop(
    "check that the location has been projected (epsg: 4326)"
  )

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
               location, z = 7, override_size_check = TRUE,
               progress = FALSE
             )
           )
           file_path <- paste0(output_dir, "/elev/srtm.tif")
           terra::writeRaster(srtm_mosaic, filename = file_path,
                              overwrite = TRUE)
         },
         { # geodata
           srtm_mosaic <- .elev_geodata(location = location)
           file_path <- paste0(output_dir, "/elev/srtm.tif")
           terra::writeRaster(srtm_mosaic, filename = file_path,
                              overwrite = TRUE)
         }
  )
}
