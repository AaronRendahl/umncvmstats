#' One-sample t-test
#'
#' Compute the mean of a sample and the corresponding confidence interval, using
#' Student's t distribution. Optionally, compute a p-value for a
#' specified null hypothesis.
#'
#' @param formula a formula of the form `~ y` or `y ~ 1`, where `y` is a numeric variable.
#'     To perform test within subgroups, use `y ~ x` or `y ~ 1 | g`, or even `y ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param backtransform if response variable is of form `log(...)`, backtransform
#'     the resulting estimate and confidence interval bounds.
#'
#' @importFrom stats t.test
#' @export
one_t_test <- function(formula, data,
                       alternative = c("two.sided", "less", "greater"),
                       null, conf.level = 0.95,
                       backtransform = TRUE) {

  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)
  do.test <- !missing(null)
  if(!do.test) { null <- 0 }

  f <- parse_formula(formula=formula, data=data)
  x <- f$data$left
  name <- f$about$var.names
  tt <- t.test(x, alternative=alternative, mu=null, conf.level=conf.level)
  result <- tidy(tt) |> mutate(SE=tt$stderr)
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals.",
                   result$method, result$alternative, conf.level*100)
  result <- result |> select(-c("method", "alternative")) |>
    rename(mean="estimate", df="parameter") |>
    mutate(.y=name, .before=1)
  if(!do.test) {
    result <- result |> select(-c("statistic", "p.value"))
  } else {
    result <- result |> mutate(null=null) |>
      rename(t.value="statistic")
  }
  result$about <- list(about)
  if(str_detect(result$.y, "^log\\(.*\\)$") && isTRUE(backtransform)) {
    result$.y <- str_replace(result$.y, "^log\\((.*)\\)$", "\\1")
    result$SE <- exp(result$mean) * result$SE
    result <- result |> mutate(across(any_of(c("mean","conf.low", "conf.high", "null")), exp))
    result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the geometric mean is reported), and the standard error is estimated using the delta method.")
  }
  as_atest(result, estimate.vars="mean", inference.vars=character())
}

#' Two-sample t-test
#'
#' Compute the difference in means between two samples, the
#' corresponding confidence interval, and a p-value for the null hypothesis
#' of equal means (that is, a difference of zero).
#'
#' @param formula a formula of the form `y ~ x`, where `y` is a numeric variable and `x` is a factor variable.
#'     To perform test within subgroups, use `y ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param var.equal a logical variable indicating whether to treat the two variances
#'     as being equal. The default is to assume unequal variance.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param backtransform if response variable is of form `log(...)`, backtransform
#'     the resulting estimate and confidence interval bounds, so that they
#'     report the ratio of the geometric means rather than the difference on the log scale.
#' @param conf.adjust adjust confidence bounds for `conf.adjust` simultaneous intervals using the Bonferroni method.
#'   Used internally by `pairwise_t_test`; should only rarely be used by users.
#'
#' @export
two_t_test <- function(formula, data,
                       alternative = c("two.sided", "less", "greater"),
                       null = 0,
                       var.equal = FALSE,
                       conf.level = 0.95, conf.adjust = 1,
                       backtransform = TRUE) {

  a <- test_by()
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)

  f <- parse_formula(formula=formula, data=data)

  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  x <- checkif2(x)
  backtransform <- str_detect(y.name, "^log\\(.*\\)$") && isTRUE(backtransform)
  use.conf.level <- 1 - (1-conf.level)/conf.adjust
  tt <- t.test(y~x, alternative=alternative, mu=null,
                   var.equal=var.equal, conf.level=use.conf.level)
  result <- broom::tidy(tt) |> mutate(SE=tt$stderr)
  adjust_txt <- if(conf.adjust!=1) sprintf(", adjusted for %d comparisons using the Bonferroni method", conf.adjust) else ""
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals%s.",
                   result$method, result$alternative, conf.level*100, adjust_txt)
  result <- result |>
    mutate(.x_contrast=paste(levels(x), collapse=if(backtransform) " / " else " - "), .after="estimate2") |>
    rename(difference="estimate", df="parameter", t.value="statistic") |>
    select(-c("estimate1", "estimate2", "method", "alternative")) |>
    mutate(null=null) |>
    relocate("null", "t.value", "df", "p.value", .after="conf.high") |>
    mutate(.y = y.name, .x = x.name)
  result$about <- list(about)
  if(backtransform) {
     result$.y <- str_replace(result$.y, "^log\\((.*)\\)$", "\\1")
     result$SE <- exp(result$difference) * result$SE
     result <- result |> mutate(across(any_of(c("difference","conf.low", "conf.high", "null")), exp)) |>
       rename(ratio="difference")
     result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the ratio is reported), and the standard error is estimated using the delta method.")
  }
  as_atest(result, estimate.vars=c("difference", "ratio"), inference.vars=c("null", "t.value"))
}

#' @param adjust method of adjusting p-values for multiple comparisons, one of "`bonferroni`", "`holm`", or "`none`".
#' @param reverse reverse the direction of pairwise comparisons.
#' @param ... further arguments to be passed to submethods, as appropriate.
#' @rdname two_t_test
#' @export
pairwise_t_test <- function(formula, data, adjust=c("bonferroni", "holm", "none"), reverse=FALSE, ...) {
  pairwise(formula, data, "two_t_test", adjust=adjust, reverse=reverse, ...)
}

#' Paired t-test
#'
#' Compute the mean difference between paired samples, the corresponding
#' confidence interval, and a p-value for the null hypothesis of equal means
#' (that is, a difference of zero).
#'
#' @param formula a formula of the form `y2 - y1 ~ 1` or `~ y2 -y1`, where `y1` and `y2` are numeric variables.
#'     To perform test within subgroups, use `y2 - y1 ~ x` or `y2 - y1 ~ 1 | g`, or even `y2- y1 ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param backtransform if response variable is of form `log(y2) - log(y1)`, backtransform
#'     the resulting estimate and confidence interval bounds, so that they
#'     report the ratio of the geometric means rather than the difference on the log scale.
#'
#' @export
paired_t_test <- function(formula, data,
                          alternative = c("two.sided", "less", "greater"),
                          null = 0,
                          conf.level = 0.95,
                          backtransform = TRUE) {
  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)

  f <- parse_formula(formula=formula, data=data, split_chars=c("+", "-"))
  if(f$about$ops[2] != "-" || nrow(f$about)!=2) stop("expected y2 - y1 in formula")
  y.names <- f$about$var.names
  backtransform <- all(str_detect(y.names, "^log\\(.*\\)$")) && isTRUE(backtransform)

  tt <- t.test(f$data$left.1, f$data$left.2, paired=TRUE)
  result <- tidy(tt) |> mutate(SE=tt$stderr)
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals.",
                   result$method, result$alternative, conf.level*100)
  if(backtransform) y.names <- str_replace(y.names, "^log\\((.*)\\)$", "\\1")
  response <- paste(y.names, collapse=if(backtransform) " / " else " - ")
  result <- result |>
    select(difference="estimate", "SE", "conf.low", "conf.high", t.value="statistic", df="parameter", "p.value") |>
    mutate(.y_contrast=response, null=null)
  result$about <- list(about)
  if(backtransform) {
    result$SE <- exp(result$difference) * result$SE
    result <- result |> mutate(across(any_of(c("difference","conf.low", "conf.high", "null")), exp)) |>
      rename(ratio="difference")
    result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the ratio is reported), and the standard error is estimated using the delta method.")
  }
  as_atest(result, estimate.vars=c("difference", "ratio"), inference.vars=c("null", "t.value"))
}
