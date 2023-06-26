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
#' @return
#' See documentation from [`chelsa()`], [`worldclim()`] and
#' [`elev()`].
#'
#' @author James L. Tsakalos
#' @seealso The underlying functions [`chelsa()`], [`worldclim()`] and
#' [`elev()`].
#'
#' @examples
#' \dontrun{
#' # Download time will depend on the size of the area you wish to access
#' # climate data for and your internet connection speed.
#'
#' # Import the Italian Biome data set
#' data("it_py", package = "climenv")
#'
#' # Run the download function
#' ce_download(
#'   output_dir = "../WorkingDirectory",
#'   c_source = "WorldClim",
#'   e_source = "mapzen",
#'   location = it_py
#' )
#'
#' }
#' @export
ce_download <- function(
    output_dir,
    c_source = "WorldClim",
    e_source = "mapzen",
    var,
    location, ...) {

  if (missing(output_dir))
    stop("Set output directory")

  if (missing(location))
    stop("Set spatial location for sourcing elevation data")

  # Partial, case-insensitive matching
  if (is.na(pmatch(toupper(c_source), c("CHELSA", "WORLDCLIM"))))
    stop("c_source must be either CHELSA, WorldClim")

  # Download CHELSA
  if ("CHELSA" %in% toupper(c_source)) {
    chelsa(output_dir = output_dir, var = var, ...)
  }

  # Download WorldClim
  if ("WORLDCLIM" %in% toupper(c_source)) {
    worldclim(output_dir = output_dir, location = location, var = var, ...)
  }

  # Download elevation
  if (!is.null(e_source)) {
    elev(output_dir = output_dir, location = location,
         e_source = e_source)
  }

}
