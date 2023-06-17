#' Download climate and elevation data
#'
#' @description
#' `ce_download()` brings together the `worldclim()`,
#' `chelsa()` and `elev()` functions to streamline downloading.
#'
#' @template output_dir_param
#' @template output_c_source_param
#' @template output_e_source_param
#' @template output_mode_param
#' @template output_var_param
#' @template output_elev_param
#' @template output_loc_param
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
#'
#' # Note that the function requires ~13.5 GB of space.
#' # Download time will depend on your internet connection speed.
#'
#' # Import the Sibillini National Park Boundary
#' data("Sibillini_py", package = "climenv")
#'
#' # Run the download function
#' ce_download(
# MS: Indented for readability
#'   output_dir = "../WorkingDirectory",
#'   c_source = "WorldClim",
#'   mode = "wb",
#'   elev = "mapzen",
#'   loc = Sibillini_py
#' )
#'
#' }
#' @export
ce_download <- function(
    output_dir,
    c_source = "CHELSA",
    e_source = "mapzen",
    mode = "wb", var = NULL,
    elev = "mapzen", loc) {

  if (missing(output_dir))
    stop("Set output directory")

  if (missing(loc))
    stop("Set spatial location for sourcing elevation data")

  # Partial, case-insensitive matching
  if (is.na(pmatch(toupper(c_source), c("CHELSA", "WORLDCLIM"))))
    stop("c_source must be either CHELSA, WorldClim")

  # Download CHELSA
  if ("CHELSA" %in% toupper(c_source)) {
    chelsa(output_dir = output_dir, mode = mode, quiet = TRUE, var = var)
  }

  # Download Worldclim
  if ("WORLDCLIM" %in% toupper(c_source)) {
    worldclim(output_dir = output_dir, quiet = TRUE, var = var)
  }

  # Download elevation
  elev(output_dir = output_dir, loc = loc,
       source = e_source)

}
