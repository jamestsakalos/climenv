#' Download WorldClim climate data
#'
#' @description
#' `worldclim()` downloads the WorldClim V2.1 climate data for 1970-2000. This
#' includes monthly climate data for minimum, mean, and maximum temperature and
#' precipitation at a resolution of 30 arc sections.
#'
#' @template output_dir_param
#' @template output_mode_param
#' @template output_quiet_param
#' @template output_var_param
#'
#' @return
#' Creates four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year
#' (January is 1; December is 12) for the time period 1970–2000. Each of the
#' files are downloaded at a spatial resolution of 30 arc seconds (~1 km  sq.).
#' The precipitation folder contains average monthly precipitation (mm). The
#' tmax folder contains maximum monthly temperature. The tmin folder contains
#' minimum monthly temperature. The tmean folder contains the average monthly
#' temperature. The unit of measure for temperature is in °C.
#'
#' @author James L. Tsakalos
#' @seealso Downloading from WorldClim V2.1 [`worldclim()`] or a more convenient
#' function for other climate and elevation data [`ce_download()`].
#' @references{ Fick, S.E. and R.J. Hijmans. (2017). WorldClim 2: new 1km
#' spatial resolution climate surfaces for global land areas. International
#' Journal of Climatology. 37, 4302–4315.
#' }
#'
#' @examples
#' \dontrun{
#' # Note that the function requires ~13.5 GB of space.
#' # Download time will depend on your internet connection speed.
#'
#'    worldclim(output_dir = "...Desktop/worldclim")
#' }
#' @importFrom utils data download.file unzip
#' @export
worldclim <- function(output_dir, mode = "wb",
                      quiet = FALSE, var = NULL) {

  # Checks the download mode, may need to change depending on operating system
  if (is.na(match(mode, c("w", "wb", "a", "ab"))))
    stop("mode must be one of w, wb, a, ab.")

  # Checks is user supplied correct climate var to download
  if (!is.null(var)) {
    if (is.na(match(var, c("prec", "tmax", "tmin", "tmean"))))
      stop("var must be one of prec, tmax, tmin, tmean.")

    vars <- c("prec", "tmax", "tmin", "tmean")
  }

  # this is the secure access website path
  site <- "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/"

  .download_dir <- function(what) {

    origOpt <- options(timeout = max(10000, getOption("timeout")))

    dir.create(paste0(output_dir, "/", what),
               recursive = TRUE, showWarnings = FALSE)

    remote_path <- paste0(site, "wc2.1_30s_", what, ".zip")

    # This part checks if the files have downloaded correctly
    # Or if the file is already containing the desired files
    if (file.size(paste0(output_dir, "/", what)) != 4096) {
      # !file.size(...) is `FALSE` (!)
      temp_path <- tempfile()
      # MS: Can we fail informatively here?
      tryCatch(
        download.file(
          remote_path,
          destfile = temp_path, mode = mode,
          cacheOK = FALSE, quiet = quiet
        ),
        error = function(e) message("Could not download from URL ", remote_path)
      )
      on.exit(unlink(temp_path))
      on.exit(options(origOpt))
      unzip(temp_path, exdir = paste0(output_dir, "/", what))
    }

    # Returning `TRUE` on success or `FALSE` on failure.
    ifelse(file.size(paste0(output_dir, "/", what)) == 4096,
           print(paste0(what, ": TRUE")),
           print(paste0(what, ": FALSE")))

  }

  # This runs through every variable which is supplied
  lapply(vars, .download_dir)

}
