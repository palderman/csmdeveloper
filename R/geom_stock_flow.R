
#
#' @export
GeomStockFlow <- ggplot2::ggproto("GeomStockFlow", ggplot2::Geom,
                             required_aes = c("x", "y", "type"),
                             default_aes = ggplot2::aes(size = 1,
                                                        colour = "black",
                                                        fill = "white",
                                                        angle = 0,
                                                        hjust = 0.5,
                                                        vjust = 0.5,
                                                        alpha = NA,
                                                        family = "",
                                                        label = "",
                                                        fontface = 1,
                                                        lineheight = 1.2),
                             # draw_key = ggplot2::draw_key_polygon,
                             draw_panel = function(data, panel_params, coord) {
                               coords <- coord$transform(data, panel_params)
                               shape_xy <- build_shape_xy(data, coords)
                               text_xy <- data[data$type == "state", , drop = FALSE]
                               # ggplot2:::ggname("geom_stock_flow",
                               #   grid::gTree(
                               #     children = grid::gList(
                                     grid::polygonGrob(
                                       shape_xy$x, shape_xy$y, id = shape_xy$id,
                                       default.units = "native",
                                       gp = grid::gpar(col = coords$colour, fill = coords$fill)
                                     )#,
                                   #   grid::textGrob(
                                   #     text_xy$label,
                                   #     x = text_xy$x, y = text_xy$y,
                                   #     default.units = "native", hjust = data$hjust,
                                   #     vjust = data$vjust, rot = data$angle,
                                   #     gp = grid::gpar(col = alpha(data$colour, data$alpha),
                                   #                     fontsize = data$size * .pt,
                                   #                     fontfamily = data$family,
                                   #                     fontface = data$fontface,
                                   #                     lineheight = data$lineheight)
                                   #   )
                                   # )
                               #   )
                               # )
                             }
)

#' @export
geom_stock_flow <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    geom = GeomStockFlow, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

