#' ce_plot
#'
#'
#' @description Creates a graph using the climate and elevation data which has
#' been extracted for a given \code{location}. It accepts the data formatted
#' from the \code{ce_extract} function.
# MS: We expect a very specific format for `data`, e.g. the first row should
# be named "location_g".  This should be specified in the params.
#' @param data List. A list storing matrices containing the mean and standard
#' deviation of the climate and/or elevation data.
#' @param c_source Character specifying source of climate data.
#' Acceptable values: \code{"CHELSA"} \code{"WorldClim"}.
# MS: We should support partial, case-insensitive matching.
#
# I don't understand what I should specify for `location_g`
#' @param location_g Character. Corresponding to the area for which the diagram
#' will be produced.
# MS: Unclear.  And: this is a weird way to set things up.
# Why not have two functions, `wl_plot()` and `holdridge_plot()`?
#' @param type Character. To produce Walter-Lieth ("WL") or Holdridge ("H")
#' diagrams
#' @param pt.col,pt.pch,\dots Arguments to control point styling in
#' `HoldridgePoints()`.
#'
#' @return Returns a 'ggplot2' family of plot. This function uses the climaemet
#' package to create the Walter and Lieth (XXXX) climatic diagram and the
#' \pkg{macroBiome} and \pkg{Ternary} packages to create a Holdridge simplex
#' plot.
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
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
#' # Is it possible to construct a small "toy" example that could be run from
#' # the data directory, for example?
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
ce_plot <- function(data, c_source, location_g, type = c("WL", "H"),
                    pt.col = "red", pt.pch = 19, ... # ... other ternary options
                    ) {

  # Set location_g as the row.names for all data
  # MS: Not clear why this is necessary
  data <- lapply(
    data, FUN = function(x) {
      row.names(x) <- x[, "location_g"]
      # MS:
      # - makes comment redundant
      # - survives if data has extra columns or funny column order
      return(x)
    }
  )

  # Check if the c_source argument is correct
  if (is.na(match(location_g, data$abmt[, 1])))
    stop(
      paste(
        c("location_g must be either:",
          paste(as.character(data$abmt[, 1]), collapse = ", ")),
        collapse = " "
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

      ggplotify::as.ggplot( # Do we need to do this?
        # If it's worth doing, we could set it as the default option and
        # allow the user to turn it off
        function() {

          # Suppress plot margins
          oPar <- par(mar = c(0, 0, 0, 0))
          on.exit(par(oPar)) # Restore initial parameters

          # Create blank Holdridge plot
          Ternary::HoldridgePlot(hex.labels = Ternary::holdridgeLifeZonesUp)
          Ternary::HoldridgeBelts()

          hold <- macroBiome::cliHoldridgePoints(
            data$tmean_m[location_g, 2:13],
            data$prec_m[location_g, 2:13],
            verbose = TRUE
          )

          # Plot the data
          # MS: might we not allow the user some flexibility in how the specify
          # these options, e.g. by allowing them to specify a pt.col option?
          Ternary::HoldridgePoints(hold$per, hold$tap,
                                   col = pt.col, cex = 2, pch = pt.pch,
                                   lwd = 2, ...)

        }
      )

    }
  )
}
