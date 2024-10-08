#' Holdridge points
#'
#' Calculates the values of bioclimatic indices from monthly temperature and
#' precipitation measurements, following Szelepcs&eacute;nyi et al. 2014.
#'
#' @param temp A matrix or data frame with 12 columns containing
#' temperatures, in degrees C, for each month of the calendar year.
#' @param prec A matrix or data frame with the same dimensions as `temp`,
#' containing precipitation measurements in mm for each month of the year.
#' @returns `bioclimate()` returns a data.frame whose columns
#' correspond to:
#'
#' - `abt`, mean annual biotemperature; mean annual temperature after
#' replacing entries outside the range 0° to 30° with zeros;
#' - `tap`, total annual precipitation (mm);
#' - `per`, potential evapotranspiration ratio;
#' - `zone`, name of associated holdridge zone
#'
#' @author Martin R. Smith
#' @references
#' Szelepcs&eacute;nyi Z, Breuer H, Sümegi P (2014)
#' The climate of Carpathian Region in the 20th century based on the original
#' and modified Holdridge life zone system.
#' *Cent Eur J Geosci* 6, 293&ndash;307. \doi{10.2478/s13533-012-0189-5}
#'
#' Holdridge L. R. (1959) Simple method for determining potential
#' evapotranspiration from temperature data. *Science*, 130, 572.
#' \doi{10.1126/science.130.3375.572}
#' @examples
#' # Step 1. Import the Italian Biome polygon data
#' # Step 2. Run the download function
#' # Step 3. Run the extract function
#' #* See ce_download & ce_extract documentation
#'
#' # Steps 1, 2 & 3 can be skipped by loading the extracted data (it_data)
#' data("it_data", package = "climenv")
#'
#' bioclimate(it_data$tavg_m[, 1:12], it_data$prec_m[, 1:12])
#' @export
bioclimate <- function(temp, prec) {
  if (!identical(dim(temp), dim(prec))) {
    stop("temp and prec should have the same dimensions")
  }
  if (dim(temp)[[2]] != 12) {
    stop("Columns of `temp` should correspond to the 12 months")
  }
  biotemp <- temp
  biotemp[temp < 0 | temp > 30] <- 0
  abt <- rowSums(biotemp / 12)
  tap <- unname(rowSums(prec))
  ape <- abt * 58.93 # Constant given in Holdridge 1959
  per <- ape / tap

  # Return:
  data.frame(abt = abt, tap = tap, per = per,
             zone = .bioclimate_zone(per, tap))
}

.bioclimate_zone <- function(per, tap) {
  hz <- Ternary::holdridge[, c("Precipitation", "PET")]
  Ternary::holdridge[apply(apply(cbind(per, tap), 1, function(x) {
    sqrt(rowSums((hz - x) ^ 2))
  }), 2, which.min), "Holdridge_Zones"]
}
