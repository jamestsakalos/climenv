#' elev
#'
#' @description
#' \code{elev} downloads elevation data the Shuttle Radar Topography Mission
#' (SRTM) , specifically the hole-filled CGIAR-SRTM (90 m resolution) for
#' latitudes between -60 and 60 or Mapzen's synthesis digital elevation product.
#'
#' @template output_dir_param
#' @param location A \code{sp}, \code{sf} polygon or point object.
#' @param source Character. Elevation sourced from mapzen (default) or geodata.
#'
#' @return
#' Returns one elevation tile (tiff). If elevation is sourced from geodata the
#' elevation is downloaded at a spatial resolution of 30 arc seconds (~1 km2).
#' If elevation data is sourced from mapzen then the product will be a mosaic.
#' Specfically, Mapzen’s product is unique in that it combines several sources
#' of digital elevation models, including SRTM, the ArcticDEM (covering all
#' areas north of 60°), EUDEM (digital elevation model over Europe; for review,
#' see Mouratidis & Ampatzidis, 2019), and others into a single product. The
#' resolution of this product was set to 7, corresponding to 611.5 m ground
#' resolution at 60° latitude 864.8 m at 45° and 1223 m at 0°.
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
#' @references{ Hijmans, R.J., et al., (2023). geodata: Download Geographic
#' Data. R package version 0.5-8. https://CRAN.R-project.org/package=geodata
#'
#' Hollister, J. W., et al. (2021). elevatr: Access Elevation Data from Various
#' APIs. R package version 0.4.2. https://CRAN.R-project.org/package=elevatr
#'
#' Mouratidis, A., and Ampatzidis, D. (2019). European Digital Elevation Model
#' Validation against Extensive Global Navigation Satellite Systems Data and
#' Comparison with SRTM DEM and ASTER GDEM in Central Macedonia (Greece).
#' ISPRS International Journal of Geo-Information 8, 108.
#' DOI: 10.3390/ijgi8030108
#' }
#'
#' @examples
#' \dontrun{
#' # Start by loading Italy's Mount Sibillini National Park boundary
#' data("Sibillini_py")
#' # elevation will be saved in the output_dir (i.e. output directory)
#' elev(output_dir = "...Desktop/elev", location = Sibillini_py)
#' }
#' @importFrom elevatr get_elev_raster
#' @importFrom geodata elevation_3s
#' @importFrom sf as_Spatial
#' @importFrom sp Polygon Polygons SpatialPolygons spTransform
#' @importFrom terra crs ext intersect mosaic rast vect
#' @export
elev <- function(output_dir = NULL, location = NULL, source = "mapzen") {

  if (is.null(output_dir))
    stop("Set output directory")

  if (is.null(location))
    stop("Set location polygon/s")

  if (is.na(match(source, c("mapzen", "geodata"))))
    stop("source must be one of mapzen or geodata")

  #Convert sf locations to SP
  if (("sf" %in% class(location)) || ("sfc" %in% class(location))) {
    location <- sf::as_Spatial(location)
  }

  # Create elev folder
  if (!dir.exists(paste0(output_dir))) {
    dir.create(paste0(output_dir),
               recursive = TRUE, showWarnings = FALSE)
  }else {
    file.remove(list.files(paste0(output_dir),
                           include.dirs = FALSE, full.names = TRUE,
               recursive = TRUE))
  }

  # Read in the SRTM tiles
  srtm_tiles <- climenv::srtm.tiles # preferable to data("srtm.tiles"); see ?data

  # Set projection
  location <- sp::spTransform(location, terra::crs(srtm_tiles))

  # Make a bounding box around the location

  bbox <- matrix(terra::ext(terra::vect(location))[c(1, 3, 2, 4)], 2, 2)
  x <- c(bbox[1, 1], bbox[1, 1], bbox[1, 2], bbox[1, 2], bbox[1, 1])
  y <- c(bbox[2, 1], bbox[2, 2], bbox[2, 2], bbox[2, 1], bbox[2, 1])
  p <- sp::Polygon(cbind(x, y))
  ps <- sp::Polygons(list(p), "p1")
  location <- sp::SpatialPolygons(list(ps), 1L,
                                  proj4string = terra::crs(srtm_tiles))

  # Intersect location and tiles
  intersects <- terra::intersect(terra::vect(location), terra::vect(srtm_tiles))
  tiles <- srtm_tiles[srtm_tiles$FID %in% intersects$FID, ]

  # Download and merge the SRTM tiles

  srtm_list <- list()

  for (i in seq_along(tiles)) {

    srtm_temp <- tempfile()

    if (source == "geodata") {
      # for  lat >= -60 & lat <= 60

      lon <- terra::ext(terra::vect(tiles[i, ]))[c(1)] +
        (terra::ext(terra::vect(tiles[i, ]))[c(2)] -
           terra::ext(terra::vect(tiles[i, ]))[c(1)]) / 2

      lat <- terra::ext(terra::vect(tiles[i, ]))[c(3)] +
        (terra::ext(terra::vect(tiles[i, ]))[c(4)] -
           terra::ext(terra::vect(tiles[i, ]))[c(3)]) / 2

      tile <- try(
        geodata::elevation_3s(lon = lon,
                              lat = lat,
                              path = srtm_temp,
                              cacheOK = FALSE)
      )
    }

    if (source == "mapzen") {
      tile <- terra::rast(
        elevatr::get_elev_raster(
          tiles[i, ], z = 7, override_size_check = TRUE,
          progress = FALSE
        )
      )
    }

    srtm_list[[i]] <- tile

  }

  # Mosaic tiles
  if (length(tiles) > 1) {
    srtm_list$fun <- mean
    srtm_mosaic <- do.call(terra::mosaic, srtm_list)
  } else {
    srtm_mosaic <- srtm_list[[1]]
  }

  # Save the mosaic
  file_path <- paste0(output_dir, "/srtm.tif")
  terra::writeRaster(srtm_mosaic, filename = file_path, overwrite = TRUE)

  # Clean the elevation folder
  rm(list = c("srtm_tiles"), envir = .GlobalEnv)

}
