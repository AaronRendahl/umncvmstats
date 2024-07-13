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

glm1 <- function(formula, data,...) {
  x <- match.call()
  x[[1]] <- quote(glm)
  out <- eval.parent(x)
  class(out) <- c("glm1", class(out))
  out
}

predictdf.glm1 <- function (model, xseq, se, level) {
  ## from ggplot:::predictdf.glm
  pred <- stats::predict(model, newdata = data_frame(x = xseq, .name_repair="minimal"),
                         se.fit = se, type = "link")
  out <- if (se) {
    std <- stats::qnorm(level/2 + 0.5)
    base::data.frame(x = xseq, y = model$family$linkinv(as.vector(pred$fit)),
                     ymin = model$family$linkinv(as.vector(pred$fit - std * pred$se.fit)),
                     ymax = model$family$linkinv(as.vector(pred$fit + std * pred$se.fit)),
                     se = as.vector(pred$se.fit))
  } else {
    base::data.frame(x = xseq, y = model$family$linkinv(as.vector(pred)))
  }
  out$y <- out$y + 1
  if(se) {
    out$ymax <- out$ymax + 1
    out$ymin <- out$ymin + 1
  }
  out
}

#' Add a smooth from a logistic regression
#'
#' @param ... XXX
#'
#' @importFrom stats binomial
#' @export
geom_smooth_binomial <- function(...) {
  geom_smooth(aes(group=1),
              method=glm1,
              formula=factor(y) ~ x,
              method.args=list(family=binomial()))
}
