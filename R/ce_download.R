#' Download climate data from XYZ
#'
#' @description
# MS: Suggest using `()` to clearly denote functions in documentation
# (optionally using markdown `dashes` to denote code - easier to type/read?
#' `ce_download()` brings together the `worldclim()`,
#' \code{chelsa} and \code{elev} functions into one location to streamline data
#' downloading.
#'
#' @param output_dir Character (e.g., "../Desktop/chelsa"). Pathway to where
#' the data will be stored.
# MS: Not true.  May be `c("CHELSA", "WorldClim")`, or `NULL`.
# MS: Why use strings here, and a logical for elev?
# For consistency,
#' @param c_source Character. Climatic data source, must be either
#' \code{"CHELSA"} or \code{"WorldClim"}.
#' @param mode Character. See documentation in [`download.file()`].
#' @param var Character. If supplied will download a subset of the climate data.
#' @param elev Logical. If `TRUE`, elevation data will be downloaded
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
# MS: is it possible to design an example that runs more efficiently:
# for example on a very small geographical area?
#
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
#'   elev = TRUE,
#'   e_source = "mapzen",
#'   e_location = Sibillini_py
#' )
#'
#' }
#' @seealso The underlying functions [`chelsa()`], [`worldclim()`] and
#' [`elev()`].
#' @export
ce_download <- function(
    output_dir, # MS: If an input is required, don't specify a default
    c_source = "NULL", # Why not default to "CHELSA" to save user keystrokes?
    mode = "wb", var = NULL,
    elev = TRUE, e_source = NULL, e_location = NULL) {

  if (missing(output_dir)) # Check becomes unnecessary if default is not specified
    stop("Set output directory")

  # Partial, case-insenstive matching makes a user's life easier
  if (is.na(pmatch(toupper(c_source), c("NULL", "CHELSA", "WORLDCLIM"))))
    stop("c_source must be either NULL, CHELSA, WorldClim")

  if (!is.null(var)) { # This check should be performed by chelsa / worldclim -
                       # thus redundant here. Delete
    if (is.na(match(var, c("prec", "tmax", "tmin", "tmean"))))
      stop("mode must be one of prec, tmax, tmin, tmean.")
  }

  if (is.na(match(mode, c("w", "wb", "a", "ab"))))
    # This check should be performed by chelsa -
    # thus redundant here. Delete
    stop("mode must be one of w, wb, a, ab. See also download.file()")

  # Suggest this syntax if it might ever be desired to download both?
  if ("CHELSA" %in% toupper(c_source)) {
    chelsa(output_dir = output_dir, mode = mode, quiet = TRUE, var = var)
  }

  if ("WORLDCLIM" %in% toupper(c_source)) {
    worldclim(output_dir = output_dir, quiet = TRUE, var = var)

  # if (!match(elev, c(TRUE, FALSE))) # Fails for elev = c(NA_logical_, TRUE, FALSE)
  if (!is.logical(elev) || length(elev) > 1) { # or avoid with isTRUE below
    stop("`elev` must be either TRUE or FALSE")
  }

  if (isTRUE(elev)) { # Saves the above check

    # Are these checks not performed by elev?  Define once, there, to avoid
    # redundancy, confusion, and later maintenance costs.
    if (is.na(match(e_source, c("mapzen", "geodata"))))
      stop("source must be one of mapzen or geodata")

    if (is.null(e_location))
      stop("Set location point/s or polygon/s for sourcing elevation data")

    # Did we use a parameter with the same name as a function?
    # Better not to - could get confusing!
    elev(output_dir = output_dir, location = e_location,
         source = e_source)
  }

}
