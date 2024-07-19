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
#' @param ... XXX
#'
#' @importFrom stats binomial
#' @export
geom_smooth_logistic <- function(...) {
  geom_smooth(method=glm,
              formula=factor(y) ~ x,
              method.args=list(family=binomial()))
}

#' Make a binary scale
#'
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
    transform=scales::new_transform("binary", tob, inverse=identity))
}
