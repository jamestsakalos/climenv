#' ce_download
#'
#' @description
#' \code{ce_download} brings together the \code{worldclim},
#' \code{chelsa} and \code{elev} functions into one location to streamline data
#' downloading.
#'
#' @param output_dir Character (e.g., ".../Desktop/chelsa"). Pathway to where
#' the data will be stored.
#' @param c_source Character. Climatic data source, must be either
#' \code{"CHELSA"} or \code{"WorldClim"}.
#' @param mode Character. See documentation in /code{download.file()}.
#' @param var Character. If supplied will download a subset of the climate data.
#' @param elev Logical. If TRUE, elevation data will be downloaded
#' @param e_source Character. Elevation sourced from mapzen (default) or
#' geodata.
#' @param e_location A \code{sp}, \code{sf} polygon or point object.
#'
#' @return
#' See documentation from \code{worldclim}, \code{chelsa} and \code{elev}.
#'
#' @author James L. Tsakalos
#'
#' @examples
#' \dontrun{
#'
#' # Note that the function requires ~13.5 GB of space.
#' # Download time will depend on your internet connection speed.
#' # Import the Sibillini National Park Boundary
#' data(Sibillini_py)
#' # Run the download function
#' ce_download(
#' output_dir = "../WorkingDirectory",
#' c_source = "WorldClim",
#' mode = "wb",
#' elev = TRUE,
#' e_source = "mapzen",
#' e_location = Sibillini_py)
#'
#' See also chelsa(), worldclim() or elev() functions.
#' }
#' @export
ce_download <- function(
    output_dir = NULL, c_source = NULL, mode = "wb", var = NULL,
    elev = TRUE, e_source = NULL, e_location = NULL) {

  if (is.null(output_dir))
    stop("Set output directory")

  if (is.na(match(c_source, c("NULL", "CHELSA", "WorldClim"))))
    stop("c_source must be either NULL, CHELSA, WorldClim")

  if (!is.null(var)) {
    if (is.na(match(var, c("prec", "tmax", "tmin", "tmean"))))
      stop("mode must be one of prec, tmax, tmin, tmean.")
  }

  if (is.na(match(mode, c("w", "wb", "a", "ab"))))
    stop("mode must be one of w, wb, a, ab. See also download.file()")

  if (c_source == "CHELSA") {
    chelsa(output_dir = output_dir, mode = mode, quiet = TRUE, var = var)
  }

  if (c_source == "WorldClim")
    worldclim(output_dir = output_dir, quiet = TRUE, var = var)

  if (!match(elev, c(TRUE, FALSE)))
    stop("elev must be either TRUE or FALSE")

  if (elev == TRUE) {

    if (is.na(match(e_source, c("mapzen", "geodata"))))
      stop("source must be one of mapzen or geodata")

    if (is.null(e_location))
      stop("Set location point/s or polygon/s for sourcing elevation data")

  }

  if (elev == TRUE)
    elev(output_dir = output_dir, location = e_location,
         source = e_source)

}
