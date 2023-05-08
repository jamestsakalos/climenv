#' ce_plot
#'
#'
#' @description Creates a graph using the climate and elevation data which has
#' been extracted for a given \code{location}. It accepts the data formatted
#' from the \code{ce_extract} function.
#' @param data list. A list storing matrices containing the mean and standard
#' deviation of the climate and/or elevation data.
#' @param c_source Character. Supply either \code{"CHELSA"} or
#' \code{"WorldClim"} argument.
#' @param location_g Character. Corresponding to the area for which the diagram
#' will be produced.
#' @param type Character. To produce Walter-Lieth ("WL") or Holdridge ("H")
#' diagrams
#'
#' @return Returns a 'ggplot2' family of plot. This function uses the climaemet
#' package to create the Walter and Lieth (XXXX) climatic diagram and the
#' macroBiome and Ternary packages to create a Holdridge simplex plot.
#'
#' @author James L. Tsakalos
#' @seealso \code{\link{ce_download.R}}
#' @references{ Holdridge (1947), Determination of world plant formations from
#' simple climatic data. Science, 105:367&ndash;368.
#' \doi{10.1126/science.105.2727.367}
#'
#' Holdridge (1967), Life Zone Ecology. Tropical Science Center, San Jos&eacute;.
#'
#' Pizarro, M, Hernang&oacute;mez, D. & Fern&aacute;ndez-Avil&eacute;s G. (2023).
#' climaemet: Climate AEMET Tools. Comprehensive R Archive Network.
#' \doi{10.5281/zenodo.5205573}
#'
#' Szelepcs&eacute;nyi, Z. (2023) macroBiome: A Tool for Mapping the
#' Distribution of the Biomes and Bioclimate. Comprehensive R Archive Network.
#' \doi{10.5281/zenodo.7633367}
#'
#' Smith, M.R (2017). Ternary: An R Package for Creating Ternary Plots.
#' Comprehensive R Archive Network. \doi{10.5281/zenodo.1068996}
#'
#' Walter, H. & Lieth, H. (1960). Klimadiagramm Weltatlas. G. Fischer, Jena.
#'
#' }
#' @encoding UTF-8
#' @examples
#' \dontrun{
#'
#' # Extraction time will depend on the size of the polygon or point file.
#' # Import the Sibillini National Park Boundary
#' data(Sibillini_py)
#' # Run the download function
#' ce_extract(
#' dir.clim = "../WorkingDirectory/CHELSA",
#' dir.elev = "../WorkingDirectory/elev",
#' location = Sibillini_py,
#' location_g = "Position"
#' c_source = "CHELSA")
#'
#' data <- ce_extract(dir.clim = dir.clim, dir.elev = dir.elev,
#' location = Sibillini_py, location_g = location_g,
#' c_source = "CHELSA", var = "ALL")
#'
#' ce_plot(data = data, c_source = "CHELSA", location_g = "High",
#' type = 'WL')
#'
#' ce_plot(data = data, c_source = "CHELSA", location_g = "High",
#' type = 'H')
#' }
#'
#' @importFrom climaemet ggclimat_walter_lieth
#' @importFrom ggplotify as.ggplot
#' @importFrom graphics par
#' @importFrom Ternary HoldridgePlot HoldridgeBelts HoldridgePoints
#' @importFrom macroBiome cliHoldridgePoints
#' @export
ce_plot <- function(data = NULL, c_source = NULL,
                    location_g = NULL, type = NULL) {

  # Set location_g as the row.names for all data
  data <- lapply(
    data, FUN = function(x) {
      row.names(x) <- x[, 1]
      return(x)
    }
  )

  # Check if the c_source argument is correct
  if (is.na(match(location_g, data$abmt[, 1])))
    stop(
      print(
        paste(
          c("location_g must be either:",
            paste(as.character(data$abmt[, 1]), collapse = ", ")),
          collapse = " "
        )
      )
    )

  # This is the switch to return the different plot types
  switch(
    type,
    "WL" = {

      climaemet::ggclimat_walter_lieth(
        dat = rbind(
          Prec. = data$prec_m[location_g, 2:13],
          Max.t. = data$tmax_m[location_g, 2:13],
          Min.t = data$tmin_m[location_g, 2:13],
          Ab.m.t = data$abmt[location_g, 2:13],
          make.row.names = TRUE
        ),
        alt = round(data$elev[location_g, 2]),
        per = switch(c_source,
                      "CHELSA" = "1981\u20132010",
                      "WorldClim" = "1970\u20132000"
        ),
        est = location_g,
        mlab = "en",
        shem = ifelse(data$lat[location_g, 2] > 0, FALSE, TRUE)
      )
    },
    "H" = {

      ggplotify::as.ggplot(
        function() {

          # Suppress plot margins
          par(mar = c(0, 0, 0, 0))

          # Create blank Holdridge plot
          Ternary::HoldridgePlot(hex.labels = Ternary::holdridgeLifeZonesUp)
          Ternary::HoldridgeBelts()

          hold <- macroBiome::cliHoldridgePoints(
            data$tmean_m[location_g, 2:13],
            data$prec_m[location_g, 2:13],
            verbose = TRUE
          )

          # Plot the data
          Ternary::HoldridgePoints(hold$per, hold$tap,
                                   col = "red", cex = 2, pch = 19,
                                   lwd = 2)

        }
      )

    }
  )
}
