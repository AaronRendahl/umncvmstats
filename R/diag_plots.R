#' Display standard diagnostic plots for a model
#'
#' Display standard diagnostic plots for a model.
#'
#' The four standard diagnostic plots created by `plot.lm` are shown, but
#' using `ggplot` graphics techniques. These plots include:
#'
#' 1) Residual vs Fitted
#'
#' 2) Normal Q-Q
#'
#' 3) Scale-Location
#'
#' 4) Residuals vs Leverage
#'
#' @param model the model to display diagnostic plots for.
#'
#' @importFrom patchwork wrap_plots
#' @importFrom broom augment
#' @export
diag_plots <- function(model) {
  augm <- broom::augment(model)
  p1 <- ggplot(augm) + aes(x=.fitted, y=.resid) +
    geom_point(pch=21) +
    geom_smooth(formula=y~x, se=FALSE, method=loess,
                method.args=list(span=1), lwd=0.75) +
    geom_hline(yintercept=0, lty=2) +
    xlab("Fitted values") + ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
  p2 <- ggplot(augm) + aes(sample=.std.resid) +
    geom_qq(pch=21) + geom_qq_line(lty=2) +
    xlab("Theoretical Quantiles") + ylab("Standardized residuals") +
    ggtitle("Normal Q-Q")
  p3 <- ggplot(augm) + aes(x=.fitted, y=sqrt(abs(.std.resid))) +
    geom_point(pch=21) +
    geom_smooth(formula=y~x, se=FALSE, method=loess,
                method.args=list(span=1), lwd=0.75) +
    xlab("Fitted values") + ylab("sqrt|Standardized residuals|") +
    ggtitle("Scale-Location")
  p4 <- ggplot(augm) + aes(x=.hat, y=.std.resid) +
    geom_point(pch=21) +
    geom_hline(yintercept=0, lty=2) +
    xlab("Leverage") + ylab("Standardized residuals") +
    ggtitle("Residuals vs Leverage")
  wrap_plots(p1, p2, p3, p4, nrow=2, byrow=TRUE)
}
