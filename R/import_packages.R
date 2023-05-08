#' @importFrom sf as_Spatial st_zm st_as_sf
#' @importFrom sp SpatialPointsDataFrame SpatialPolygonsDataFrame spTransform
#' Polygon Polygons SpatialPolygons
#' @importFrom glue glue
#' @importFrom terra rast extract vect crs ext intersect mosaic writeRaster
#' @importFrom httr HEAD
#' @importFrom geodata elevation_3s
#' @importFrom elevatr get_elev_raster
#' @importFrom fs file_temp
#' @importFrom dplyr %>% group_by summarize
utils::globalVariables(c("Name"))
NULL
