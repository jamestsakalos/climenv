#' Download CHELSA climate data
#'
#' @description
#' `chelsa()` downloads the CHELSA (Climatologies at high resolution for the
#'  earth’s land surface areas) V2.1 data of temperature and precipitation at a
#'  resolution of 30 arc sections.
#'
#' @template output_dir_param
#' @template output_var_param
#' @param quiet,mode,\dots Arguments to control a download from the Internet
#' `download.file()`.
#'
#' @return
#' Returns four subfolders named prec, tmax, tmin and tmean. Each folder
#' contains 12 GeoTiff (.tif) files, one for each month of the year for the time
#' period 1981&ndash;2010. Each of the files are downloaded at a spatial
#' resolution of 30 arc seconds (&sim;1 km sq.). The precipitation folder
#' contains average monthly precipitation (mm). The tmax folder contains maximum
#' monthly temperature. The tmin folder contains minimum monthly temperature.
#' The tmean folder contains the average monthly temperature. The unit of
#' measure for temperature is in &deg;C.
#'
#' @author James L. Tsakalos
#' @seealso Downloading from WorldClim V2.1 [`worldclim()`] or a more convenient
#' function for other climate and elevation data [`ce_download()`].
#' @references{ Karger,  D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H.,
#' Soria-Auza, R.W. et al (2017) Climatologies at high resolution for the
#' earth’s land surface areas. Scientific Data, 4, 170122.
#' \doi{10.1038/sdata.2017.122}
#'
#' Karger, D.N., Conrad, O., Böhner, J., Kawohl, T., Kreft, H., Soria-Auza,
#' R.W. et al. (2021) Climatologies at high resolution for the earth’s land
#' surface areas. EnviDat. \doi{10.16904/envidat.228.v2.1}
#'
#' }
#' @examples
#' \dontrun{
#'
#' # Download time will depend on the size of the area you wish to access
#' # climate data for and your internet connection speed.
#'
#' # Download the WorldClim data
#' chelsa(
#'   output_dir = "...Desktop/chelsa"
#' )
#'
#' # Note that unlike worldclim() we do not specify the location argument
#' # because it is not yet possible to extract smaller tile sections
#'
#' }
#' @export
chelsa <- function(output_dir = NULL, var = "all",
                   mode = "wb", quiet = FALSE, ...) {

  if (is.null(output_dir))
    stop("Set output directory")

  # If you're sitting on a train with bad wifi, maybe we need to have a
  # longer default timeout
  options(timeout = 1000)

  # I want to use these as a template, to apply correct names later.
  var_t <- c("prec", "tmax", "tmin", "tavg")
  names(var_t) <- c("pr", "tasmin", "tasmax", "tas")

  # Checks var, if an argument is provided it makes sure its correct.
  # If var is null then it assigns all the values.
  if (is.null(var) || var == "all") {
    var <- c("prec", "tmax", "tmin", "tavg")
    names(var) <- c("pr", "tasmin", "tasmax", "tas")
  } else {
    stopifnot(var %in% c("prec", "tmax", "tmin", "tavg"))

    # create clean names
    var_c <- c("prec", "tmax", "tmin", "tavg")
    names(var_c) <- c("pr", "tasmin", "tasmax", "tas")
    var <- var_c[var_c %in% var]

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

      # Download the file
      try(
        download.file(
          layer_url, file_path,
          mode = mode, quiet = quiet, ...
        )
      )
    }

  }

}
