#' plot_h
#'
#'
#' @description Creates a graph using the climate and elevation data which has
#' been extracted for a given \code{location}. It accepts the data formatted
#' from the \code{ce_extract} function.
#' @template output_data_param
#' @template output_geo_id_param
#' @param col,pch,\dots Arguments to control point styling in
#' `HoldridgePoints()`.
#'
#' @return Returns a base R family of plot. This function uses the
#' \pkg{macroBiome} and \pkg{Ternary} packages to create a Holdridge simplex
#' plot.
#'
#' @author James L. Tsakalos
#' @seealso Download climate data: [`ce_download()`]
#' @references{ Holdridge (1947), Determination of world plant formations from
#' simple climatic data. *Science*, 105:367&ndash;368.
#' \doi{10.1126/science.105.2727.367}
#'
#' Holdridge (1967), _[Life zone ecology]_. Tropical Science Center,
#' San Jos&eacute;: Tropical Science Center.
#' [Life zone ecology]: https://reddcr.go.cr/sites/default/files/centro-de-documentacion/holdridge_1966_-_life_zone_ecology.pdf
#'
#' Szelepcs&eacute;nyi, Z. (2023) macroBiome: A Tool for Mapping the
#' Distribution of the Biomes and Bioclimate. Comprehensive R Archive Network.
#' \doi{10.5281/zenodo.7633367}
#'
#' Smith, M.R (2017). Ternary: An R Package for Creating Ternary Plots.
#' Comprehensive R Archive Network. \doi{10.5281/zenodo.1068996}
#'
#' }
#' @encoding UTF-8
#' @examples{
#'
#' # Step 1. Import the Italian Biome polygon data
#' # Step 2. Run the download function
#' # Step 3. Run the extract function
#' #* See ce_download & ce_extract documentation
#'
#' # Steps 1, 2 & 3 can be skipped by loading the extracted data (it_data)
#' data("it_data", package = "climenv")
#'
#' # Step 4. Visualise the climatic envelope using a Holdridge diagram
#'
#' plot_h(data = it_data, geo_id = "MED")
#'
#' }
#'
#' @importFrom graphics par
#' @importFrom Ternary HoldridgePlot HoldridgeBelts HoldridgePoints
#' @importFrom macroBiome cliHoldridgePoints
#' @export
plot_h <- function(data, geo_id, col = "red", pch = 19,
                   ... # ... other ternary options
) {

  # Check if the c_source argument is correct
  if (is.na(match(geo_id, row.names(data[[1]])))) {
    stop(
      paste(
        c("geo_id must be either:",
          paste(as.character(row.names(data[[1]]), collapse = ", ")),
          collapse = " "
        )
      )
    )
  }

  # Holdridge climate diagram

  # I want to export as a ggplot object for consistent output classes.
  # Suppress plot margins
  opar1 <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit(graphics::par(opar1)) # Restore initial parameters

  # Create blank Holdridge plot
  Ternary::HoldridgePlot(hex.labels = Ternary::holdridgeLifeZonesUp)
  Ternary::HoldridgeBelts()

  hold <- macroBiome::cliHoldridgePoints(
    data$tavg_m[geo_id, 1:12],
    data$prec_m[geo_id, 1:12],
    verbose = TRUE
  )

  # Plot the data
  # The user has some flexibility in how to specify the point options
  Ternary::HoldridgePoints(hold$per, hold$tap,
                           col = col, cex = 2, pch = pch,
                           lwd = 2, ...)

}
