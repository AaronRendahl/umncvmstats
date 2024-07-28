#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to
#'  reduce overplotting. Based on `ggbeeswarm::geom_beeswarm`, but
#'  with a default of a compact swarm with random priority.
#'  Uses the beeswarm package.
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
#' @inheritParams offset_beeswarm
#' @import ggplot2
#' @seealso
#'  [ggbeeswarm::geom_quasirandom()] an alternative method,
#'  [beeswarm::swarmx()] how spacing is determined,
#'  [ggplot2::geom_point()] for regular, unjittered points,
#'  [ggplot2::geom_jitter()] for jittered points,
#'  [ggplot2::geom_boxplot()] for another way of looking at the conditional
#'     distribution of a variable
#' @export
geom_beeswarm <- function(
  mapping = NULL,
  data = NULL,
  stat = 'identity',
  ...,
  method = "compactswarm",
  spacing = 1,
  side = 0L,
  priority = "random",
  fast = TRUE,
  dodge.width = NULL,
  corral = "none",
  corral.width = 0.9,
  orientation = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {

  if (!method %in% c("swarm", "compactswarm", "hex", "square", "centre", "center")) {
    stop(sprintf("The method must be one of: swarm, compactswarm, hex, square, center, or centre."))
  }
  if (!corral %in% c("none", "gutter", "wrap", "random", "omit")) {
    stop(sprintf("The corral argument must be one of: none, gutter, wrap, random, or omit."))
  }

  position <- position_beeswarm(
    method = method,
    spacing = spacing,
    side = side,
    priority = priority,
    fast = fast,
    dodge.width = dodge.width,
    orientation = orientation,
    corral = corral,
    corral.width = corral.width
  )

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
