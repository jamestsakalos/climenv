##' Sibillini polygon data
##'
##' The package contains geospatial point data capturing Italy's Monti
##' 'Sibillini' National Park.
##'
##' @docType data
##' @format{ An object of class \code{sf} (inherits from \code{tbl_df},
##'  \code{tbl}, \code{data.frame}) with 328 rows and 2 columns.
##' }
##' @details Contains two polygons dividing the park into regions of 'high' and
##' 'low' altitude.
##' @references{ Mucina, L., Div&iacute;&scaron;ek, J., Tsakalos, J.L. (2022).
##' Europe, Ecosystems of. Reference Module in Life Sciences.
##' \doi{10.1016/B978-0-12-822562-2.00059-1}
##' }
##' @encoding UTF-8
##' @keywords datasets
##' @rdname Sibillini_py
##' @examples
##' data("Sibillini_py", package = "climenv")
##' terra::plot(Sibillini_py)
"Sibillini_py"
