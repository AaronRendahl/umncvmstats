#' Title
#'
#' @param mapping XX
#' @param data XX
#' @param stat XX
#' @param position XX
#' @param ... XX
#' @param just XX
#' @param width XX
#' @param na.rm XX
#' @param orientation XX
#' @param show.legend XX
#' @param inherit.aes XX
#'
#' @return XX
#' @export
geom_bars <- function (mapping = NULL, data = NULL, stat = "count",
                       position = position_dodge2(preserve="single"),
                       ..., just = 0.5, width = NULL, na.rm = FALSE, orientation = NA,
                       show.legend = NA, inherit.aes = TRUE)
{
  layer(data = data, mapping = mapping, stat = stat, geom = GeomBar,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = rlang::list2(just = just, width = width, na.rm = na.rm,
                              orientation = orientation, ...))
}
