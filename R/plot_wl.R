#' plot_wl
#'
#'
#' @description Creates a graph using the climate and elevation data which has
#' been extracted for a given \code{location}. It accepts the data formatted
#' from the \code{ce_extract} function.
#' @param data List. A list storing matrices containing the mean and standard
#' deviation of the climate and/or elevation data.
#' @template output_location_g_param
#' @param \dots Arguments to control styling in
#' `ggclimat_walter_lieth()`.
#'
#' @return Returns a 'ggplot2' family of plot. This function uses the
#' \pkg{climaemet} package to create the Walter and Lieth (1960) climatic
#' diagram.
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
#' @references{ Pizarro, M, Hernang&oacute;mez, D. &
#' Fern&aacute;ndez-Avil&eacute;s G. (2023). climaemet: Climate AEMET Tools.
#' Comprehensive R Archive Network. \doi{10.5281/zenodo.5205573}
#'
#' Walter, H. & Lieth, H. (1960). Klimadiagramm Weltatlas. G. Fischer, Jena.
#'
#' }
#' @encoding UTF-8
#' @examples{
#'
#' # Step 1. Import the Sibillini National Park Boundary
#' # Step 2. Run the download function
#' # Step 3. Run the extract function
#' #* See ce_download & ce_extract documentation
#'
#' # Steps 1, 2 & 3 can be skipped by loading the extracted data
#' data(s_data)
#'
#' # Step 4. Visualise the climatic envelope using a Walter-Lieth diagram
#'
#' plot_wl(data = s_data, location_g = "High")
#'
#' }
#'
#' @importFrom climaemet ggclimat_walter_lieth
#' @export
plot_wl <- function(data, location_g, ...) {

  # Needed to print the climate date range on the plot
  c_source <- strsplit(data$Readme, " ")[[1]][5]

  # Check if the c_source argument is correct
  if (is.na(match(location_g, row.names(data[[1]])))) {
    stop(
      paste(
        c("location_g must be either:",
          paste(as.character(row.names(data[[1]]), collapse = ", ")),
          collapse = " "
        )
      )
    )
  }

  # Walter-Lieth climate diagram

  climaemet::ggclimat_walter_lieth(
    dat = rbind(
      Prec. = data$prec_m[location_g, 1:12],
      Max.t. = data$tmax_m[location_g, 1:12],
      Min.t = data$tmin_m[location_g, 1:12],
      Ab.m.t = data$abmt[location_g, 1:12],
      make.row.names = TRUE
    ),
    alt = round(data$elev[location_g, "mean"]),
    per = switch(c_source,
                 "CHELSA" = "1981\u20132010",
                 "WorldClim" = "1970\u20132000"
    ),
    est = location_g,
    mlab = "en",
    shem = ifelse(data$lat[location_g, ] > 0, FALSE, TRUE),
    ...

  )

}
