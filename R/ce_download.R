#' Download climate and elevation data
#'
#' @description
#' `ce_download()` brings together the `worldclim()`,
#' `chelsa()` and `elev()` functions to streamline downloading.
#'
#' @template output_dir_param
#' @template output_c_source_param
#' @template output_e_source_param
#' @template output_var_param
#' @template output_location_param
#' @param \dots Arguments to control a download from the Internet
#' `download.file()`.
#'
#' @returns
#' See documentation from [`chelsa()`], [`worldclim()`] and
#' [`elev()`].
#'
#' @author James L. Tsakalos and Martin R. Smith
#' @seealso The underlying functions [`chelsa()`], [`worldclim()`] and
#' [`elev()`].
#'
#' @examples
#' \donttest{
#' # Download time will depend on the size of the area you wish to access
#' # climate data for and your internet connection speed.
#'
#' # Make a polygon file
#' regents <- sf::st_polygon(
#'   list(
#'     cbind(
#'       "lon" = c(51.537, 51.525, 51.523, 51.530, 51.534, 51.537),
#'       "lat" = c(-0.150, -0.145, -0.156, -0.167, -0.163, -0.150)
#'     )
#'   )
#' )
#'
#' # Create temporary output directory
#' temp_path <- tempdir()
#'
#' # Run the download function
#' ce_download(
#'   output_dir = temp_path,
#'   location = regents
#' )
#'
#' # Reset user options
#' unlink(file.path(temp_path))
#'
#' }
#' @export
ce_download <- function(
    output_dir,
    c_source = "WorldClim",
    e_source = "mapzen",
    var = "all",
    location, ...) {

  if (missing(output_dir)) {
    stop("Set output directory")
  }

  if (missing(location)) {
    stop("Set spatial location for sourcing elevation data")
  }

  # Partial, case-insensitive matching
  c_source_id <- pmatch(toupper(c_source), c("CHELSA", "WORLDCLIM"))
  if (any(is.na(c_source_id))) {
    warning("Unrecognized value in c_source: ",
            paste(c_source[is.na(c_source_id)], collapse = ", "),
            " (choose CHELSA or WorldClim)")
  }

  # Download CHELSA
  if (1 %in% c_source_id) {
    chelsa(output_dir = output_dir, var = var, ...)
  }

  # Download WorldClim
  if (2 %in% c_source_id) {
    worldclim(output_dir = output_dir, location = location, var = var, ...)
  }

  # Download elevation
  if (!is.null(e_source)) {
    elev(output_dir = output_dir, location = location,
         e_source = e_source)
  }

}
