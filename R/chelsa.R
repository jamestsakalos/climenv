#' chelsa
#'
#' @description
#' \code{chelsa} downloads the CHELSA (Climatologies at high resolution for the
#'  earth’s land surface areas) V2.1 data of downscaled temperature and
#'  precipitation to a resolution of 30 arc sections.
#'
#' @param output_dir Character (e.g., ".../Desktop/chelsa"). Pathway to where
#' the data will be stored.
#' @param mode Character. See documentation in /code{download.file()}.
#' @param quiet Logical. If TRUE, suppress status messages (if any), and the
#' progress bar.
#'
#' @return
#' Returns four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year
#' (January is 1; December is 12) for the time period 1981–2010. Each of the
#' files are downloaded at a spatial resolution of 30 arc seconds (~1 km sq.).
#' The precipitation folder contains average monthly precipitation (mm). The
#' tmax folder contains maximum monthly temperature. The tmin folder contains
#' minimum monthly temperature. The tmean folder contains the average monthly
#' temperature. The units for all temperature derived layers, as provided by
#' CHELSA Version 1.2, are multiplied by a factor of 10; hence, the unit of
#' measure for temperature is in °C X 10.. Specifically, monthly values of
#' minimum, maximum and mean.
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
#' @references{ Karger, D.N., et al. (2016). CHELSA climatologies at high
#' resolution for the earth’s land surface areas (Version 1.0). World Data
#' Center for Climate. DOI: 10.1594/WDCC/CHELSA_v1
#'
#' Karger, D.N., et al. (2016). CHELSA climatologies at high resolution for the
#' earth’s land surface areas (Version 1.1). World Data Center for Climate. DOI:
#' 10.1594/WDCC/CHELSA_v1_1
#'
#' Karger, D.N., et al. (2018). Data from: Climatologies at high
#'  resolution for the earth’s land surface areas. EnviDat.
#'  DOI: 10.16904/envidat.228.v2.1
#' }
#'
#' @examples
#' \dontrun{
#' chelsa(output_dir = "...Desktop/Chelsa", quiet = FALSE)
#' }
#' @export
chelsa <- function(output_dir = NULL, mode = "wb",
                   quiet = FALSE, var = NULL) {

  if (is.na(match(mode, c("w", "wb", "a", "ab"))))
    stop("mode must be one of w, wb, a, ab.")

  if (is.null(output_dir))
    stop("Set output directory")

  # If you're sitting on a train with bad wifi, maybe we need to have a
  # longer default timeout
  options(timeout = 1000)

  # I want to use these as a template, to apply correct names later.
  var_t <- c("prec", "tmax", "tmin", "tmean")
  names(var_t) <- c("pr", "tasmin", "tasmax", "tas")


  if (!is.null(var)) {

    if (is.na(match(var, c("prec", "tmax", "tmin", "tmean"))))
      stop("mode must be one of prec, tmax, tmin, tmean.")

    var <- var_t[var_t %in% var] # to assign the names

  } else {
    var <- c("prec", "tmax", "tmin", "tmean")
    names(var) <- c("pr", "tasmin", "tasmax", "tas")
  }

  # Make the folders
  for (path in var) {
    if (!dir.exists(paste0(output_dir, "/", path)))
      dir.create(paste0(output_dir, "/", path),
                 recursive = TRUE, showWarnings = FALSE)
  }

  # this is the main website path
  site <- "https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/"
  site_date <- "climatologies/1981-2010/"
  # adds the 0 before then number
  layerf <- sprintf("%02d", 1:12)
  # ending path
  site_end <- "_1981-2010_V.2.1.tif"

  for (url in seq_along(var)) {

    path_url <- names(var)[url]
    path_file <- var[url]

    for (layer in layerf){
      layer_url <- paste0(
        site, site_date, path_url, "/CHELSA_", path_url, "_", layer, site_end
      )

      file_path <- paste0(
        output_dir, "/", path_file, "/",  path_url, "_", layer, site_end
      )

      # So that we do not download the file if it already exists
      if (isFALSE(file.exists(file_path))) {
        try(
          download.file(
            layer_url, file_path,
            mode = mode, cacheOK = FALSE, quiet = quiet
          )
        )
      } else {
        print(paste0(file_path, " exists, please delete or specify new folder"))
      }

    }

  }

}
