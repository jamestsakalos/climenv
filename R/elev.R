.elev_geodata <- function(location, output_dir) {

  y_max <- 80
  y_min <- -70
  tile_degrees <- 5
  # create SRTM tiles
  rs <- terra::rast(nrows = (y_max - y_min) / tile_degrees,
                    ncols = 360 / tile_degrees,
                    xmin = -180, xmax = 180,
                    ymin = y_min, ymax = y_max)
  rs[] <- seq_len(prod(dim(rs)))

  # Intersect location and tiles
  tiles <- unique(terra::extract(rs, terra::vect(location))$lyr.1)
  srtm_points <- terra::xyFromCell(rs, tiles)

  # Make an empty list to fill
  srtm_list <- list()

  # lats
  lats <- srtm_points[, "y"]

  # Downloads the tiles and stores into that list
  for (pts in 1:seq_along(lats)) {

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
#' [`ce_download()`].
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
#' # Start by loading Italy's Biom data
#' data("italy_py", package = "climenv")
#' # elevation will be saved in the output_dir (i.e. output directory)
#'    elev(output_dir = "...Desktop/elev", location = italy_py)
#' }
#' @importFrom elevatr get_elev_raster
#' @importFrom geodata elevation_3s
#' @importFrom sf as_Spatial
#' @importFrom terra rast extract xyFromCell mosaic writeRaster rast
#' @export
elev <- function(output_dir, location, e_source = "mapzen") {

  e_source_id <- pmatch(tolower(e_source[1]), c("mapzen", "geodata"))
  if (is.na(e_source_id)) {
    stop("e_source must be \"mapzen\" or \"geodata\"")
  }

  # Convert sf locations to SP
  if (("sf" %in% class(location)) || ("sfc" %in% class(location))) {
    location <- sf::as_Spatial(location)
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
