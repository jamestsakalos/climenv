#' Italian Biome extracted data
#'
#' The package contains climatic data extracted for the Biomes of Italy.
#'
#' @docType data
#' @format{ An object of class \code{list} of length 12.}
#' @details Returns a 11 data frames and one 'Readme' note. Eight feature
#' climate data (i.e., one each for the mean and standard deviation of tmax,
#' tmean, tmin and prec). Within each data frame the columns represent a month
#' (Jan–Dec), each row represents a feature of the \code{location} \code{sp},
#' \code{sf} polygon or point object. Values returned are either degrees
#' Celsius for (tmax, tmean, tmin) or mm (prec). One data frame features
#' elevation data. Within this data frame, one column shows the mean and the
#' second the standard deviation. One data frame contains the central latitude
#' of each feature. One, called "abmt" features the absolute minimum
#' temperature for each month. For more details on the assignment of Biomes
#' please refer to Mucina et al. (2023).
#' @references{ Mucina, L., Div&iacute;&scaron;ek, J., & Tsakalos, J.L. (2023)
#' Europe, Ecosystems of. In: Encyclopedia of biodiversity, vol X (in print).
#' \doi{10.1016/B978-0-12-822562-2.00059-1}
#' }
#' @keywords datasets
#' @rdname it_data_extended
#' @examples
#' data("it_data_extended", package = "climenv")
#' head(it_data_extended$abmt)
"it_data_extended"
