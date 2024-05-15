#' draw_key_source <- function(data, params, size){
#'   grid::grobTree(ggplot2::draw_key_polygon(data, list()))
#' }
#'
#' GeomSource <- ggproto("GeomSource", ggplot2::Geom,
#'                       required_aes = c("x", "y"),
#'                       default_aes = ggplot2::aes(fill = "white", color = "black", size = 1),
#'                       draw_key = ggplot2::draw_key_polygon,
#'                       draw_panel = function(data, panel_params, coord){
#'                         coords <- coord$transform(data, panel_params)
#'                         grid::polygonGrob(coords$x, coords$y,
#'                                           gp = grid::gpar(col = coords$colour, fill = coords$fill))
#'                       })
#'
#' GeomSimplePoint <- ggplot2::ggproto("GeomSimplePoint", ggplot2::Geom,
#'                            required_aes = c("x", "y"),
#'                            default_aes = ggplot2::aes(shape = 19, colour = "black"),
#'                            draw_key = ggplot2::draw_key_point,
#'
#'                            draw_panel = function(data, panel_params, coord) {
#'                              coords <- coord$transform(data, panel_params)
#'                              grid::pointsGrob(
#'                                coords$x, coords$y,
#'                                pch = coords$shape,
#'                                gp = grid::gpar(col = coords$colour)
#'                              )
#'                            }
#' )
#'
#' #' @export
#' #'
#' geom_simple_point <- function(mapping = NULL, data = NULL, stat = "identity",
#'                               position = "identity", na.rm = FALSE, show.legend = NA,
#'                               inherit.aes = TRUE, ...) {
#'   ggplot2::layer(
#'     geom = GeomSimplePoint, mapping = mapping,  data = data, stat = stat,
#'     position = position, show.legend = show.legend, inherit.aes = inherit.aes,
#'     params = list(na.rm = na.rm, ...)
#'   )
#' }
