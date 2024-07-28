#' Bar charts
#'
#' Make a bar chart with `ggplot`, with bars within a group next to each other by
#' default (that is, "dodged") instead of stacked. Based on `ggplot::geom_bar`.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping
#'   at the top level of the plot. You must supply `mapping` if there is no plot
#'   mapping.
#' @param data The data to be displayed in this layer. There are three
#'    options:
#'
#'    If `NULL`, the default, the data is inherited from the plot
#'    data as specified in the call to [ggplot()].
#'
#'    A `data.frame`, or other object, will override the plot
#'    data. All objects will be fortified to produce a data frame. See
#'    [fortify()] for which variables will be created.
#'
#'    A `function` will be called with a single argument,
#'    the plot data. The return value must be a `data.frame`, and
#'    will be used as the layer data. A `function` can be created
#'    from a `formula` (e.g. `~ head(.x, 10)`).
#' @param stat Override the default connection between `geom_bar()` and
#'   `stat_count()`. For more information about overriding these connections,
#'   see how the [stat][layer_stats] and [geom][layer_geoms] arguments work.
#' @param position A position adjustment to use on the data for this layer. This
#'   can be used in various ways, including to prevent overplotting and
#'   improving the display. The `position` argument accepts the following:
#'   * The result of calling a position function, such as `position_jitter()`.
#'     This method allows for passing extra arguments to the position.
#'   * A string naming the position adjustment. To give the position as a
#'     string, strip the function name of the `position_` prefix. For example,
#'     to use `position_jitter()`, give the position as `"jitter"`.
#'   * For more information and other ways to specify the position, see the
#'     [layer position][layer_positions] documentation.
#' @param ... Other arguments passed on to [layer()]'s `params` argument.
#' @param width Bar width. By default, set to 90% of the `resolution()` of the data.
#' @param na.rm If `FALSE`, the default, missing values are removed with
#'   a warning. If `TRUE`, missing values are silently removed.
#' @param orientation The orientation of the layer. The default (`NA`)
#'   automatically determines the orientation from the aesthetic mapping. In the
#'   rare event that this fails it can be given explicitly by setting `orientation`
#'   to either `"x"` or `"y"`. See the *Orientation* section for more detail.
#' @param just Adjustment for column placement. Set to `0.5` by default, meaning
#'   that columns will be centered about axis breaks. Set to `0` or `1` to place
#'   columns to the left/right of axis breaks. Note that this argument may have
#'   unintended behaviour when used with alternative positions, e.g.
#'   `position_dodge()`.
#' @param show.legend logical. Should this layer be included in the legends?
#'   `NA`, the default, includes if any aesthetics are mapped.
#'   `FALSE` never includes, and `TRUE` always includes.
#'   It can also be a named logical vector to finely select the aesthetics to
#'   display.
#' @param inherit.aes If `FALSE`, overrides the default aesthetics,
#'   rather than combining with them. This is most useful for helper functions
#'   that define both data and aesthetics and shouldn't inherit behaviour from
#'   the default plot specification, e.g. [borders()].
#'
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
