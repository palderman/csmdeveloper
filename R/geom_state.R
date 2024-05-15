#' @export
GeomState <- ggplot2::ggproto("GeomState", ggplot2::GeomLabel,
                              required_aes = c("x", "y", "label"),
                              default_aes = ggplot2::aes(label.r = unit(0, "line"),
                                                         colour = "black",
                                                         fill = "white"))

#' @export
geom_state <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
          ..., parse = FALSE, nudge_x = 0, nudge_y = 0, label.padding = unit(0.25, "lines"),
          label.r = unit(0.15, "lines"), label.size = 0.25,
          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
{
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c("both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
                       i = "Only use one approach to alter the position"))
    }
    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomLabel,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = rlang::list2(parse = parse, label.padding = label.padding,
                       label.r = label.r, label.size = label.size, na.rm = na.rm,
                       ...))
}
