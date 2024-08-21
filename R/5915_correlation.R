#' Correlation Tests and Confidence Intervals
#'
#' @param formula A formula of the form `y~x`
#' @param data A data frame containing the values in the formula
#' @param alternative options
#' @param method options
#' @param conf.level The desired confidence level for the confidence intervals.
#' @param ... Additional parameters
#'
#' @importFrom stats cor.test
#' @export
correlation_inference <- function(formula, data,
                             alternative = c("two.sided", "less", "greater"),
                             method = c("pearson", "kendall", "spearman"),
                             conf.level = 0.95, ...) {

  a <- test_by()
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)
  method <- match.arg(method)

  f <- parse_formula(formula=formula, data=data)

  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  capture_result <- capture_warnings(
    cor.test(x, y, alternative=alternative, method=method, conf.level=conf.level, ...),
    "Cannot compute exact p-value with ties", "Ties detected, p-value not exact.")
  result <- capture_result$result |> tidy()
  if("conf.low" %in% names(result)) {
    ci.txt <- sprintf(", with %0.0f%% confidence intervals", conf.level*100)
  } else {
    ci.txt <- ""
  }
  about <- sprintf("%s (%s)%s", result$method, result$alternative, ci.txt)
  result <- result |> select(correlation="estimate", any_of(c("conf.low", "conf.high")), "p.value") |>
    mutate(.y=y.name, .x=x.name)
  result$about <- list(c(about, capture_result$warnings))
  as_atest(result, estimate.vars=c("correlation"), inference.vars=character())
}
