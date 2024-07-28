#' Hide the y-axis of a ggplot
#'
#' @export
hide_y_axis <- function() {
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
}

#' Add a smooth from a logistic regression
#'
#' Add a smooth to a `ggplot`, using a logistic model.
#'
#' Uses `geom_smooth` with method `glm` and the `binomial` family, and a formula
#' of `factor(y) ~ x`.
#'
#' @param mapping Set of aesthetic mappings created by [aes()]. If specified and
#'   `inherit.aes = TRUE` (the default), it is combined with the default mapping at
#'   the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. See `ggplot::geom_smooth` for additional details.
#' @param se logical specifying if confidence interval should be shown around the smooth.
#' @param ... other arguments passed to `geom_smooth`.
#' @importFrom stats glm
#' @importFrom stats binomial
#' @export
geom_smooth_logistic <- function(mapping=NULL, data=NULL,
                                 se=TRUE, ...) {
  geom_smooth(mapping=mapping, data=data,
              method=glm,
              formula=factor(y) ~ x,
              method.args=list(family=binomial()), se=se, ...)
}

#' Make a binary scale
#'
#' @param ... Additional parameters, passed to scale_y_continuous
#' @importFrom scales new_transform
#' @export
scale_y_binary <- function(...) {
  tob <- function(x) {
    if(is.factor(x)) {
      (x!=levels(x)[1])*1.0
    } else {
      x
    }
  }
  scale_y_continuous(
    transform=new_transform("binary", tob, inverse=identity), ...)
}
