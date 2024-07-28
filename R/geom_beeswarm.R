#' Points, jittered to reduce overplotting using the beeswarm package
#'
#' The beeswarm geom is a convenient means to offset points within categories to
#'  reduce overplotting. Based on `ggbeeswarm::geom_beeswarm`, but
#'  with a default of a compact swarm with random priority.
#'  Uses the beeswarm package.
#'
#' @param data A data.frame containing plotting data in columns x and y.
#' Usually obtained from data processed by ggplot2.

#' @param method Method for arranging points (see Details below)
#' @param spacing Scaling for adjusting point spacing (see [beeswarm::swarmx()]).
#' Values between 1 (default) and 3 tend to work best.
#' @param side Direction to perform jittering: 0: both directions;
#' 1: to the right or upwards; -1: to the left or downwards.
#' @param priority Method used to perform point layout (see Details below)
#' @param fast Use compiled version of swarm algorithm? This option is ignored
#' for all methods expect `"swarm"` and `"compactswarm"`.
#' @param corral `string`. Method used to adjust points that would be placed to
#' wide horizontally, default is `"none"`. See details below.
#' @param corral.width `numeric`. Width of the corral, default is `0.9`.
#'
#' @details
#' **method:** specifies the algorithm used to avoid overlapping points. The
#' default `"swarm"` method places points in increasing order. If a point would
#' overlap with an existing point, it is shifted sideways (along the group axis)
#' by a minimal amount sufficient to avoid overlap.
#'
#' While the `"swarm"` method places points in a predetermined
#' order, the `"compactswarm"` method uses a greedy strategy to determine which
#' point will be placed next. This often leads to a more tightly-packed layout.
#' The strategy is very simple: on each iteration, a point that can be placed as
#' close as possible to the non-data axis is chosen and placed. If there are two
#' or more equally good points, `priority` is used to break ties.
#'
#' The other 3 methods first discretise the values along the data axis, in order
#' to create more efficient packing. The `"square"` method places points on a
#' square grid, whereas `"hex"` uses a hexagonal grid. `"centre"`/`"center"`
#' uses a square grid to produce a symmetric swarm. The number of break points
#' for discretisation is determined by a combination of the available plotting
#' area and the `spacing` argument.
#'
#' **priority:** controls the order in which points are placed, which generally
#' has a noticeable effect on the plot appearance. `"ascending"` gives the
#' 'traditional' beeswarm plot. `"descending"` is the opposite. `"density"`
#' prioritizes points with higher local density. `"random"` places points in a
#' random order. `"none"` places points in the order provided.
#'
#' **corral:** By default, swarms from different groups are not prevented from
#' overlapping, i.e. `"corral = "none"`. Thus, datasets that are very large or
#' unevenly distributed may produce ugly overlapping beeswarms. To control
#' runaway points one can use the following methods. `"gutter"` collects runaway
#' points along the boundary between groups. `"wrap"` implement periodic boundaries.
#' `"random"` places runaway points randomly in the region. `"omit"` omits runaway
#' points.
#'
#' @keywords internal
#' @importFrom beeswarm swarmx
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom rlang inform
#' @seealso [geom_beeswarm()], [ggbeeswarm::position_quasirandom()],
#' [beeswarm::swarmx()]
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams position_beeswarm
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
