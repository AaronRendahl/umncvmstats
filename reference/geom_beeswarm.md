# Points, jittered to reduce overplotting using the beeswarm package

The beeswarm geom is a convenient means to offset points within
categories to reduce overplotting. Based on `ggbeeswarm::geom_beeswarm`,
but with a default of a compact swarm with random priority. Uses the
beeswarm package.

## Usage

``` r
geom_beeswarm(
  mapping = NULL,
  data = NULL,
  stat = "identity",
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
)

position_beeswarm(
  method = "compactswarm",
  spacing = 1,
  side = 0L,
  priority = "random",
  fast = TRUE,
  orientation = NULL,
  dodge.width = 0,
  corral = "none",
  corral.width = 0.2
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  A data.frame containing plotting data in columns x and y. Usually
  obtained from data processed by ggplot2.

- stat:

  The statistical transformation to use on the data for this layer. When
  using a `geom_*()` function to construct a layer, the `stat` argument
  can be used the override the default coupling between geoms and stats.
  The `stat` argument accepts the following:

  - A `Stat` ggproto subclass, for example `StatCount`.

  - A string naming the stat. To give the stat as a string, strip the
    function name of the `stat_` prefix. For example, to use
    `stat_count()`, give the stat as `"count"`.

  - For more information and other ways to specify the stat, see the
    [layer
    stat](https://ggplot2.tidyverse.org/reference/layer_stats.html)
    documentation.

- ...:

  Other arguments passed on to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html)'s
  `params` argument. These arguments broadly fall into one of 4
  categories below. Notably, further arguments to the `position`
  argument, or aesthetics that are required can *not* be passed through
  `...`. Unknown arguments that are not part of the 4 categories below
  are ignored.

  - Static aesthetics that are not mapped to a scale, but are at a fixed
    value and apply to the layer as a whole. For example,
    `colour = "red"` or `linewidth = 3`. The geom's documentation has an
    **Aesthetics** section that lists the available options. The
    'required' aesthetics cannot be passed on to the `params`. Please
    note that while passing unmapped aesthetics as vectors is
    technically possible, the order and required length is not
    guaranteed to be parallel to the input data.

  - When constructing a layer using a `stat_*()` function, the `...`
    argument can be used to pass on parameters to the `geom` part of the
    layer. An example of this is
    `stat_density(geom = "area", outline.type = "both")`. The geom's
    documentation lists which parameters it can accept.

  - Inversely, when constructing a layer using a `geom_*()` function,
    the `...` argument can be used to pass on parameters to the `stat`
    part of the layer. An example of this is
    `geom_area(stat = "density", adjust = 0.5)`. The stat's
    documentation lists which parameters it can accept.

  - The `key_glyph` argument of
    [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html) may
    also be passed on through `...`. This can be one of the functions
    described as [key
    glyphs](https://ggplot2.tidyverse.org/reference/draw_key.html), to
    change the display of the layer in the legend.

- method:

  Method for arranging points (see Details below)

- spacing:

  Scaling for adjusting point spacing (see
  [`beeswarm::swarmx()`](https://rdrr.io/pkg/beeswarm/man/swarmx.html)).
  Values between 1 (default) and 3 tend to work best.

- side:

  Direction to perform jittering: 0: both directions; 1: to the right or
  upwards; -1: to the left or downwards.

- priority:

  Method used to perform point layout (see Details below)

- fast:

  Use compiled version of swarm algorithm? This option is ignored for
  all methods expect `"swarm"` and `"compactswarm"`.

- dodge.width:

  Amount by which points from different aesthetic groups will be dodged.
  This requires that one of the aesthetics is a factor.

- corral:

  `string`. Method used to adjust points that would be placed to wide
  horizontally, default is `"none"`. See details below.

- corral.width:

  `numeric`. Width of the corral, default is `0.9`.

- orientation:

  The orientation (i.e., which axis to group on) is inferred from the
  data. This can be overridden by setting `orientation` to either `"x"`
  or `"y"`.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes. It can also be a named logical
  vector to finely select the aesthetics to display.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

## Details

**method:** specifies the algorithm used to avoid overlapping points.
The default `"swarm"` method places points in increasing order. If a
point would overlap with an existing point, it is shifted sideways
(along the group axis) by a minimal amount sufficient to avoid overlap.

While the `"swarm"` method places points in a predetermined order, the
`"compactswarm"` method uses a greedy strategy to determine which point
will be placed next. This often leads to a more tightly-packed layout.
The strategy is very simple: on each iteration, a point that can be
placed as close as possible to the non-data axis is chosen and placed.
If there are two or more equally good points, `priority` is used to
break ties.

The other 3 methods first discretise the values along the data axis, in
order to create more efficient packing. The `"square"` method places
points on a square grid, whereas `"hex"` uses a hexagonal grid.
`"centre"`/`"center"` uses a square grid to produce a symmetric swarm.
The number of break points for discretisation is determined by a
combination of the available plotting area and the `spacing` argument.

**priority:** controls the order in which points are placed, which
generally has a noticeable effect on the plot appearance. `"ascending"`
gives the 'traditional' beeswarm plot. `"descending"` is the opposite.
`"density"` prioritizes points with higher local density. `"random"`
places points in a random order. `"none"` places points in the order
provided.

**corral:** By default, swarms from different groups are not prevented
from overlapping, i.e. `"corral = "none"`. Thus, datasets that are very
large or unevenly distributed may produce ugly overlapping beeswarms. To
control runaway points one can use the following methods. `"gutter"`
collects runaway points along the boundary between groups. `"wrap"`
implement periodic boundaries. `"random"` places runaway points randomly
in the region. `"omit"` omits runaway points.

## See also

`geom_beeswarm()`, `ggbeeswarm::position_quasirandom()`,
[`beeswarm::swarmx()`](https://rdrr.io/pkg/beeswarm/man/swarmx.html)

`ggbeeswarm::geom_quasirandom()` an alternative method,
[`beeswarm::swarmx()`](https://rdrr.io/pkg/beeswarm/man/swarmx.html) how
spacing is determined,
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
for regular, unjittered points,
[`ggplot2::geom_jitter()`](https://ggplot2.tidyverse.org/reference/geom_jitter.html)
for jittered points,
[`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html)
for another way of looking at the conditional distribution of a variable

`geom_beeswarm()`, `ggbeeswarm::position_quasirandom()`,
[`beeswarm::swarmx()`](https://rdrr.io/pkg/beeswarm/man/swarmx.html)
