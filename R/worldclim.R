#' worldclim
#'
#' @description
#' \code{worldclim} downloads the WorldClim V2 data.
#'
#' @param output_dir Character (e.g., ".../Desktop/chelsa"). Pathway to where
#' the data will be stored.
#' @param mode Character. See documentation in /code{download.file()}.
#' @param quiet Logical. If TRUE, suppress status messages (if any), and the
#' progress bar.
#' @param var Character. If supplied will download a subset of the climate data.
#'
#' @return
#' Returns four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year
#' (January is 1; December is 12) for the time period 1970–2000. Each of the
#' files are downloaded at a spatial resolution of 30 arc seconds (~1 km2).
#' The precipitation folder contains average monthly precipitation (mm). The
#' tmax folder contains maximum monthly temperature. The tmin folder contains
#' minimum monthly temperature. The tmean folder contains the average monthly
#' temperature. The units for all temperature derived layers, as provided by
#' WorldClim2, are provided in °C.
#'
#' @author James L. Tsakalos
#' @seealso \code{\link{ce_download.R}}
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
#' worldclim(output_dir = "...Desktop/worldclim")
#' }
#' @export
worldclim <- function(output_dir = NULL, mode = "wb",
                      quiet = FALSE, var = NULL) {

  if (is.na(match(mode, c("w", "wb", "a", "ab"))))
    stop("mode must be one of w, wb, a, ab.")

  if (!is.null(var)) {
    if (is.na(match(var, c("prec", "tmax", "tmin", "tmean"))))
      stop("mode must be one of prec, tmax, tmin, tmean.")

    vars <- c("prec", "tmax", "tmin", "tmean")
  }

  if (is.null(output_dir))
    stop("Set output directory")

  # If you're sitting on a train with bad wifi, maybe we need to have a
  # longer default timeout
  options(timeout = 10000)

  # this is the main website path
  site <- "http://biogeo.ucdavis.edu/data/worldclim/v2.1/base/"


  for (i in vars) {

    switch(
      EXPR = i,
      "prec" = {
        dir.create(paste0(output_dir, "/prec"),
                   recursive = TRUE, showWarnings = FALSE)
        prec_path <- paste0(site, "wc2.1_30s_prec.zip")

        # This part checks if the files have downloaded correctly
        if (!file.size(paste0(output_dir, "/prec")) == 4096) {
          prec_temp <- tempfile()
          try(download.file(
            prec_path,
            destfile = prec_temp, mode = mode,
            cacheOK = FALSE, quiet = quiet
          ))
          unzip(prec_temp, exdir = paste0(output_dir, "/prec"))
          rm(prec_temp)
        }
      },
      "tmin" = {
        dir.create(paste0(output_dir, "/tmin"),
                   recursive = TRUE, showWarnings = FALSE)
        tmin_path <- paste0(site, "wc2.1_30s_tmin.zip")

        if (!file.size(paste0(output_dir, "/tmin")) == 4096) {
          tmin_temp <- tempfile()

          try(download.file(
            tmin_path,
            destfile = tmin_temp, mode = mode,
            cacheOK = FALSE, quiet = quiet
          ))
          unzip(tmin_temp, exdir = paste0(output_dir, "/tmin"))
          rm(tmin_temp)
        }
      },
      "tmax" = {
        dir.create(paste0(output_dir, "/tmax"),
                   recursive = TRUE, showWarnings = FALSE)
        tmax_path <- paste0(site, "wc2.1_30s_tmax.zip")

        if (!file.size(paste0(output_dir, "/tmax")) == 4096) {
          tmax_temp <- tempfile()
          try(download.file(
            tmax_path,
            destfile = tmax_temp, mode = mode,
            cacheOK = FALSE, quiet = quiet
          ))
          unzip(tmax_temp, exdir = paste0(output_dir, "/tmax"))
          rm(tmax_temp)
        }
      },
      "tmean" = {
        dir.create(paste0(output_dir, "/tmean"),
                   recursive = TRUE, showWarnings = FALSE)
        tmean_path <- paste0(site, "wc2.1_30s_tavg.zip")

        if (!file.size(paste0(output_dir, "/tmean")) == 4096) {
          tmean_temp <- tempfile()
          try(download.file(
            tmean_path,
            destfile = tmean_temp, mode = mode,
            cacheOK = FALSE, quiet = quiet)
          )
          unzip(tmean_temp, exdir = paste0(output_dir, "/tmean"))
          rm(tmean_temp)
        }
      }
    )
  }
}
