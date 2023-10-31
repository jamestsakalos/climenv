#' Italian Biome polygon data
#'
#' The package contains geospatial polygon data showing the Biomes of Italy.
#'
#' @docType data
#' @format{ An object of class \code{sf} (inherits from \code{tbl_df},
#'  \code{tbl}, \code{data.frame}) with 328 rows and 2 columns.
#' }
#' @details The first column, \code{geometry}, refers to the X and Y coordinate
#' of each polygon spaced over Italy. The second column, \code{GB}, refers to
#' the Global Biome assigned to each coordinate. For more details on the
#' assignment of Biomes please refer to Mucina et al. (2023).
#' @references{ Mucina, L., Div&iacute;&scaron;ek, J., & Tsakalos, J.L. (2023)
#' Europe, Ecosystems of. In: Encyclopedia of biodiversity, vol X (in print).
#' \doi{10.1016/B978-0-12-822562-2.00059-1}
#' }
#' @encoding UTF-8
#' @keywords datasets
#' @rdname it_py
#' @examples
#' data("it_py", package = "climenv")
#' plot(it_py)
"it_py"
