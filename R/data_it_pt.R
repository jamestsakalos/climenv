##' Italian Biome point data
##'
##' The package contains geospatial point data showing the Biomes of Italy.
##'
##' @docType data
##' @format{ An object of class \code{sf} (inherits from \code{tbl_df},
##' \code{tbl}, \code{data.frame}) with 9963 rows and 2 columns.
##' }
##' @details The first column, \code{geometry}, refers to the X and Y coordinate
##' of each point spaced over Italy. The second column, \code{GB}, refers to
##' the Global Biome assigned to each coordinate. For more details on the
##' assignment of Biomes please refer to Mucina et al. (2022).
##' @references{ Mucina, L., Div&iacute;&scaron;ek, J., Tsakalos, J.L. (2022).
##' Europe, Ecosystems of. Reference Module in Life Sciences.
##' \doi{10.1016/B978-0-12-822562-2.00059-1}
##' }
##' @encoding UTF-8
##' @keywords datasets
##' @rdname it_pt
##' @examples
##' data("it_pt")
##' plot(it_pt)
"it_pt"
