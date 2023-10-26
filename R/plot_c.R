.intervalm <- function(a, b) {
  seq(from = min(a), to = max(a), by = (max(a) - min(a)) / b)
}

.concat_text <- function(x, y, txt, col, font = 1, pfont = 1) {
  thisx <- x
  for (txtstr in 1:seq_along(txt)) {
    text(thisx, y, txt[txtstr],
         col = col[txtstr],
         adj = c(0, 1),
         cex = pfont * 0.9, font = font)
    thisx <- thisx + strwidth(txt[txtstr]) * pfont * 0.9
  }
}

.lat_check <- function(data, geo_id) {
  if (data$lat[geo_id, ] == 0) {
    stop(
      "invalid latitude for `geo_id` ",
      "must be positive or negative"
    )
  }
}

#' plot_c
#'
#' @description Creates a graph using the climate and elevation data which has
#' been extracted for a given \code{location}. It accepts the data formatted
#' from the \code{ce_extract} function.
#' @template output_data_param
#' @template output_geo_id_param
#' @param interval_prec Numeric. Interval of the precipitation axis.
#' @param interval_temp Numeric. Interval of the temperature axis.
#' @param stretch_temp Numeric. Ratio for stretching temperature relative to
#' precipitation.
#' @param p_cex Numeric. Text size for entire plot.
#' @param nchr_main Numeric. Controls the number of characters of the main title
#' before wrapping to a new line.
#' @param l_main Numeric. Line position of the main title.
#' @param l_prec Numeric. Line position of the y axis (precipitation).
#' @param l_month Numeric. Line position of the x axis (months).
#' @param lwd_temp Numeric. Line width of temperature.
#' @param lwd_prec Numeric. Line width of precipitation.
#' @param l_units Numeric. Line position of precipitation and temperature units.
#' @param l_tcols List. Line position of the table columns. Must be length 5
#' corresponding to the position (left to right) for each column.
#'
#' @returns
#' Returns a base R family of plot. This function uses the \pkg{dismo} package
#' to calculate isothermality (ISO), temperature seasonality (TS) and
#' precipitation seasonality (PS).
#'
#' @author James L. Tsakalos and Martin R. Smith
#' @seealso Download climate data: [`ce_download()`]
#' @references
#' Pizarro, M, Hernang&oacute;mez, D. &amp;
#' Fern&aacute;ndez-Avil&eacute;s G. (2023). climaemet: Climate AEMET Tools.
#' Comprehensive R Archive Network. \doi{10.5281/zenodo.5205573}
#'
#' Walter, H.B., & Lieth, H. (1960). Klimadiagramm-Weltatlas. VEB Gustav
#' Fischer Verlag, Jena.
#'
#' @encoding UTF-8
#' @examples
#' # Step 1. Import the Italian Biome polygon data
#' # Step 2. Run the download function
#' # Step 3. Run the extract function
#' #* See ce_download & ce_extract documentation
#'
#' # Steps 1, 2 & 3 can be skipped by loading the extracted data (it_data)
#' data("it_data", package = "climenv")
#'
#' # Step 4. Visualise the climatic envelope using our custom diagram
#'
#' # first lets store the default graphics parameters
#' # we need to make some changes to ensure that the table fits in the plotting
#' # region.
#'
#' # Set up plotting parameters
#' oldpar <- par(mar = c(1.5, 2.2, 1.5, 14) + 0.01)
#'
#' plot_c(
#'   it_data, geo_id = "MED",
#'   l_tcols = c(14.5, 17, 18.5, 19.5, 21)
#' )
#'
#' on.exit(par(oldpar))
#' # This output works if you export to a three column width sized image.
#'
#' @importFrom dismo biovars
#' @importFrom plyr round_any
#' @importFrom graphics axis mtext points strwidth text
#' @importFrom stats sd
#' @export
plot_c <- function(data,
                   geo_id,
                   interval_prec = 20,
                   interval_temp = 5,
                   stretch_temp = 4,
                   p_cex = 1,
                   nchr_main = 32,
                   l_main = 0.1,
                   l_prec = 0,
                   l_month = -0.6,
                   lwd_temp = 1.5,
                   lwd_prec = 9,
                   l_units = 0.1,
                   l_tcols = c(14.5, 16.5, 18.5, 19.5, 22.5)) {

  # Check that the latitude falls in the northern or southern hemisphere.
  # geo_id can not fall on the equator
  .lat_check(data, geo_id)

  # Set the names of t_col
  names(l_tcols) <- c(paste0("L", 1:5))

  # Start by getting the Holdridge information. Specifically: abt, Mean Annual
  # Biotemperature; tap, Total Annual Precipitation (in mm); per, Potential
  # Evapotranspiration Ratio
  hold <- bioclimate(data$tavg_m[geo_id, 1:12], data$prec_m[geo_id, 1:12])

  # I also need to calculate the bioclimatic variables
  bioclim <- dismo::biovars(
    prec = unlist(data$prec_m[geo_id, 1:12]),
    tmin = unlist(data$tmin_m[geo_id, 1:12]),
    tmax = unlist(data$tmax_m[geo_id, 1:12])
  )
  row.names(bioclim) <- geo_id

  #* scaling precipitation ####

  y_min <- c(min(data$tmin_m) * stretch_temp) * 1.1
  y_min <- plyr::round_any(abs(y_min), 10, f = ceiling) * -1
  y_min <- y_min * stretch_temp
  y_max <- max(
    c(
      max(data$prec_m * 1.1),
      max(data$tmax_m[geo_id, ] * stretch_temp) * 1.1
    )
  )

  # Precipitation bars (plot) ####
  plot(c(1:12), data$prec_m[geo_id, ], type = "h", lwd = lwd_prec,
       col = "blue", yaxs = "i", lend = 1,
       ylim = c(y_min, y_max),
       xaxt = "n", xlab = "", ylab = "", yaxt = "n", xpd = FALSE)

  mtext(paste(strwrap(geo_id, width = nchr_main), collapse = "\n"),
        side = 3, line = l_main, font = 2, cex = p_cex * .7)

  axis(side = 2, at = seq(0, max(data$prec_m), by = interval_prec),
       col.axis = "blue", las = 1, cex.axis = p_cex * .8, line = l_prec)

  axis(side = 1, at = c(1:12),
       labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"),
       tick = FALSE, cex.axis = p_cex, line = l_month, gap.axis = 0.3)
  axis(side = 1, at = seq(0.5, 12.5, by = 1), labels = FALSE)

  # Temperature lines (plot) ####
  par(new = TRUE, xpd = FALSE)

  plot(c(1:12), data$tmax_m[geo_id, ] * stretch_temp, type = "l",
       axes = FALSE, col = "red", lwd = lwd_temp,
       ylim = c(y_min, y_max),
       xlab = "", ylab = "", yaxs = "i")

  points(c(1:12), data$tmin_m[geo_id, ] * stretch_temp, type = "l",
         col = "red", lwd = lwd_temp)

  #* scaling temperature ####
  t_min <- plyr::round_any(abs(y_min), 10, f = ceiling) * -1
  t_max <- max(data$tmax_m)
  t_max <- plyr::round_any(max(t_max), 10, f = ceiling)
  t_min <- t_min * stretch_temp
  t_max <- t_max * stretch_temp

  axis(side = 4, at = seq(t_min, t_max, by = (interval_temp * stretch_temp)),
       labels = seq(t_min, t_max,
                    by = (interval_temp * stretch_temp)) / stretch_temp,
       las = 2, col.axis = "red",
       cex.axis = p_cex * .8, line = -0.2, tick = FALSE)
  axis(side = 4, at = seq(t_min, t_max, by = (interval_temp * stretch_temp)),
       las = 2, labels = FALSE)

  mtext("mm", side = 3, line = l_units,
        at = -0.75, col = "blue", cex = p_cex * .6)
  mtext("\u00B0C", side = 3, line = l_units,
        at = 13.5, col = "red", cex = p_cex * .6)

  # This part works out how to distribute the tabular information evenly
  ht <- rev(
    .intervalm(
      c(
        y_min,
        y_max
      ), 10
    )
  )

  # Side table ####
  par(xpd = TRUE)

  #* Temperature ####

  text <- c("BioT")
  .concat_text(l_tcols["L1"], ht[1], text, col = "black", font = 2)
  text <- round(hold[, "abt"], 2)
  .concat_text(l_tcols["L2"], ht[1], text, col = c(rep("red", 4)))
  text <- c("\u00B0C")
  .concat_text(l_tcols["L5"], ht[1], text, col = "red")

  text <- c("ISO")
  .concat_text(l_tcols["L1"], ht[2], text, col = "black", font = 2)
  text <- round(bioclim[geo_id, 3], 2)
  .concat_text(l_tcols["L2"], ht[2], text, col = c(rep("red", 4)))
  text <- c("%")
  .concat_text(l_tcols["L5"], ht[2], text, col = "red")

  text <- c("MAT")
  .concat_text(l_tcols["L1"], ht[3], text, col = "black", font = 2)
  text <- c(round(mean(as.numeric(data$tavg_m[geo_id, ])), 1))
  .concat_text(l_tcols["L2"], ht[3], text, col = "red")
  text <- c("\u00B1")
  .concat_text(l_tcols["L3"], ht[3], text, col = c(rep("red", 4)))
  text <- c(round(sd(as.numeric(data$tavg_m[geo_id, ])), 1))
  .concat_text(l_tcols["L4"], ht[3], text, col = c(rep("red", 4)))
  text <- c("\u00B0C")
  .concat_text(l_tcols["L5"], ht[3], text, col = "red")

  text <- c("TS")
  .concat_text(l_tcols["L1"], ht[4], text, col = "black", font = 2)
  text <- round(bioclim[geo_id, 4], 2)
  .concat_text(l_tcols["L2"], ht[4], text, col = "red")
  text <- c("%")
  .concat_text(l_tcols["L5"], ht[4], text, col = "red")

  #* Precipitation ####

  text <- c("Dry mo")
  .concat_text(l_tcols["L1"], ht[5], text, col = "black", font = 2)
  text <- c(
    length(as.numeric(data$prec_m[geo_id, which(data$prec_m[geo_id, ] < 50)]))
  )
  .concat_text(l_tcols["L3"], ht[5], text, col = "blue")
  text <- c("No.")
  .concat_text(l_tcols["L5"], ht[5], text, col = "blue")

  text <- c("MAP")
  .concat_text(l_tcols["L1"], ht[6], text, col = "black", font = 2)
  text <- c(round(sum(as.numeric(data$prec_m[geo_id, ])), 0))
  .concat_text(l_tcols["L2"], ht[6], text, col = "blue")
  text <- c("\u00B1")
  .concat_text(l_tcols["L3"], ht[6], text, col = c(rep("blue", 3)))
  text <- c(round(sd(data$prec_m[geo_id, ]), 0))
  .concat_text(l_tcols["L4"], ht[6], text, col = c(rep("blue", 3)))
  text <- c("mm")
  .concat_text(l_tcols["L5"], ht[6], text, col = "blue")

  text <- c("PET")
  .concat_text(l_tcols["L1"], ht[7], text, col = "black", font = 2)
  text <- round(hold[, "per"], 2)
  .concat_text(l_tcols["L2"], ht[7], text, col = c(rep("blue", 4)))
  text <- cut(
    text, c(
      0.015625, 0.03125, 0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32, Inf
    ),
    labels = c(
      "Saturated", "Subsaturated", "Semisaturated", "Superhumid", "Perhumid",
      "Humid", "Subhumid", "Semiarid", "Arid", "Perarid",
      "Superarid", "Semiparched"
    )
  )
  text <- paste0("[", text, "]")
  .concat_text(l_tcols["L3"], ht[7], text, col = "blue", pfont = 0.7)
  text <- c("ratio")
  .concat_text(l_tcols["L5"], ht[7], text, col = "blue")

  text <- c("PS")
  .concat_text(l_tcols["L1"], ht[8], text, col = "black", font = 2)
  text <- round(bioclim[geo_id, 15], 2)
  .concat_text(l_tcols["L2"], ht[8], text, col = "blue")
  text <- c("%")
  .concat_text(l_tcols["L5"], ht[8], text, col = c(rep("blue", 4)))


  if (data$lat[geo_id, ] > 0) {
    text <- c("S:A:W:V")
    .concat_text(l_tcols["L1"], ht[9], text, col = "black", font = 2)
    text <- c(
      round(
        sum(as.numeric(data$prec_m[geo_id, c(6, 7, 8)])) /
          sum(as.numeric(data$prec_m[geo_id, ])) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(9, 10, 11)])) /
          sum(as.numeric(data$prec_m[geo_id, ])) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(12, 1, 2)])) /
          sum(as.numeric(data$prec_m[geo_id, ])) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(3, 4, 5)])) /
          sum(as.numeric(data$prec_m[geo_id, ])
          ) * 100, 0
      )
    )
    .concat_text(l_tcols["L2"] + 1.15, ht[9],
                 paste(text, collapse = ""), col = "blue")
    text <- c("%")
    .concat_text(l_tcols["L5"], ht[9], text, col = "blue")
  }

  if (data$lat[geo_id, ] < 0) {
    text <- c("S:A:W:V")
    .concat_text(l_tcols["L1"], ht[9], text, col = "black", font = 2)
    text <- c(
      round(
        sum(as.numeric(data$prec_m[geo_id, c(12, 1, 2)])) /
          sum(as.numeric(data$prec_m[geo_id, ])
          ) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(3, 4, 5)])) /
          sum(as.numeric(data$prec_m[geo_id, ])
          ) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(6, 7, 8)])) /
          sum(as.numeric(data$prec_m[geo_id, ])
          ) * 100, 0
      ), ":",
      round(
        sum(as.numeric(data$prec_m[geo_id, c(9, 10, 11)])) /
          sum(as.numeric(data$prec_m[geo_id, ])
          ) * 100, 0
      )
    )

    .concat_text(l_tcols["L2"] + 1.15, ht[9],
                 paste(text, collapse = ""), col = "blue")
    text <- c("%")
    .concat_text(l_tcols["L5"], ht[9], text, col = "blue")
  }

  #* Elevation and latitude ####

  text <- c("Elv")
  .concat_text(l_tcols["L1"], ht[10], text, col = "black", font = 2)
  text <- round(c(data$elev[geo_id, 1]))
  .concat_text(l_tcols["L2"], ht[10], text, col = c(rep("black", 4)))
  text <- c("\u00B1")
  .concat_text(l_tcols["L3"], ht[10], text, col = c(rep("black", 2)))
  text <- round(c(data$elev[geo_id, 2]), 0)
  .concat_text(l_tcols["L4"], ht[10], text, col = c(rep("black", 2)))
  text <- c("m")
  .concat_text(l_tcols["L5"], ht[10], text, col = "black")

  text <- c("Lat")
  .concat_text(l_tcols["L1"], ht[11], text, col = "black", font = 2)
  text <- round(c(data$lat[geo_id, 1]), 2)

  .concat_text(l_tcols["L2"], ht[11], text, col = c(rep("black", 4)))

  # This part makes S or N depending on the latitude.
  if (text < 0) {
    text <- c("\u00B0S")
  } else {
    text <- c("\u00B0N")
  }

  .concat_text(l_tcols["L5"], ht[11], text, col = "black")

}
