#' Two-sample proportion test
#'
#' Computes the absolute difference in proportion between two samples, and reports a
#' confidence interval for this difference and a p-value for the null hypothesis
#' of equal proportions (that is, a difference of zero).
#'
#' Two methods are currently supported, the asymptotic test using the chi-squared
#' statistic (either with or without continuity correction) or Fisher's exact test.
#' By default, the chi-squared method is used, with continuity correction determined
#' as in the [stats::prop.test] function, unless the minimum expected count under the
#' null of equal proportions is less than 5, in which case Fisher's exact test is used.
#'
#' When Fisher's exact test is used, no confidence interval for the difference
#' in proportions is reported.
#'
#' The chi-squared option uses [stats::prop.test] and the exact test option uses
#' [stats::fisher.test].
#'
#' @param formula a formula of the form `y ~ x`, where `y` and `x` are a factor variables.
#'     If not a factor, it will be automatically converted.
#'     To perform test within subgroups, use `y ~ x | g`.
#' @param data a data frame containing the values in the formula.
#' @param success an optional value specifying the level for which proportions should be reported.
#' @param method character string specifying which method to use. One of "`default`", "`chisq`", or "`exact`".
#' @param alternative  character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param ... further arguments to be passed to submethods, as appropriate.
#'
#' @return A tibble with class `atest` containing columns as follows:
#' \item{difference}{the difference in proportion between the two groups}
#' \item{conf.low}{lower confidence bound}
#' \item{conf.high}{upper confidence bound}
#' \item{chisq.value}{the chi-squared value (if chi-squared method used)}
#' \item{p.value}{the p-value of the test}
#'
#' @export
two_proportion_test <- function(x, ...) { UseMethod("two_proportion_test") }

#' @rdname two_proportion_test
#' @export
two_proportion_test.formula <- function(formula, data, success, method=c("default", "chisq", "exact"),
                                        alternative = c("two.sided", "less", "greater"), ...) {

  a <- test_by()
  if(!is.null(a)) return(a)

  method <- match.arg(method)
  alternative <- match.arg(alternative)
  f <- parse_formula(formula=formula, data=data)
  if(!formula_has(f,1,1,0)) stop("improper formula; expecting y~x")
  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  y <- checkif2(y, require_two = FALSE)
  x <- checkif2(x)
  if(missing(success)) {
    if(nlevels(y)==2) {
      success <- levels(y)[2]
    } else {
      success <- levels(y)[1]
    }
  }
  stopifnot(success %in% levels(y))
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  m <- table(x, y)
  result <- two_proportion_test.default(m, alternative=alternative, method=method, ...)
  result <- result |> mutate(.y = y.name, .y_value=success, .before=1) |>
    mutate(.x = x.name,
           .x_contrast=paste(levels(x), collapse=" - "))
  as_atest(result)
}

#' @param x vector with count of successes in the two groups
#' @param n vector with count of total trials in the two groups
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param conf.adjust adjust confidence bounds for `conf.adjust` simultaneous intervals using the Bonferroni method.
#'   Used internally by `pairwise_proportion_test`; should only rarely be used by users.
#' @param correct a logical indicating whether Yates' continuity correction should be applied; used for Wilson test only.
#' @param ... further arguments to be passed to submethods, as appropriate.
#'
#' @importFrom stats fisher.test
#' @importFrom stats prop.test
#' @rdname two_proportion_test
#' @export
two_proportion_test.default <- function(x, n,
                                        method=c("default", "chisq", "exact"),
                                        alternative = c("two.sided", "less", "greater"),
                                        conf.level = 0.95, conf.adjust=1, correct = TRUE,
                                        ...) {
  use.conf.level <- 1 - (1-conf.level)/conf.adjust
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  if(!missing(n)) { m <- cbind(x, n-x) } else { m <- x }
  E <- outer(rowSums(m), colSums(m))/sum(m)
  computed.diff <- unname(diff(m[,1]/rowSums(m)))
  if((method=="default" && any(E < 5)) || method=="exact") {
    ft <- fisher.test(m, alternative=alternative) |> tidy()
    about <- c(sprintf("%s (%s)", ft$method, ft$alternative))
    if(method!="exact") {
      about <- c(about, "Method chosen due to expected counts < 5.")
    }
    result <- ft |> select("p.value") |> mutate(difference=computed.diff, .before="p.value")
    result$about <- list(about)
  } else {
    capture <- capture_warnings(
      prop.test(x=x, n=n, alternative=alternative,
                conf.level=use.conf.level, correct=correct),
      "Chi-squared approximation may be incorrect",
      "At least one expected count < 5; chi-squared approximation may be incorrect.")
    result <- capture$result |> broom::tidy()
    adjust_txt <- if(conf.adjust!=1) sprintf(", adjusted for %d comparisons using the Bonferroni method", conf.adjust) else ""
    about <- sprintf("%s (%s), with %0.0f%% confidence intervals%s.",
                     result$method, result$alternative, conf.level*100, adjust_txt)
    about <- c(about, capture$warnings)
    result <- result |>
      mutate(difference=.data$estimate2 - .data$estimate1, .before=1) |>
      select(-c("method", "alternative", "parameter", "estimate1", "estimate2")) |>
      rename(chisq.value="statistic") |>
      relocate(c("chisq.value", "p.value"), .after="conf.high")
  }
  result$about <- list(about)
  as_atest(result, estimate.vars="difference", inference.vars="chisq.value")
}

#' @rdname two_proportion_test
#' @param adjust method of adjusting p-values for multiple comparisons, one of "`bonferroni`", "`holm`", or "`none`".
#' @param reverse reverse the direction of pairwise comparisons.
#' @export
pairwise_proportion_test <- function(formula, data, adjust=c("bonferroni", "holm", "none"), reverse=FALSE) {
  adjust <- match.arg(adjust)
  pairwise(formula, data, "two_proportion_test", adjust=adjust, reverse=reverse, ...)
}

#' Paired proportion test (McNemar's)
#' @param formula XX
#' @param data XX
#' @param success XX
#' @param alternative XX
#' @param correct XX
#' @param conf.level XX
#' @param method XX
#'
#' @export
paired_proportion_test <- function(formula, data, success,
                                   alternative = c("two.sided", "less", "greater"),
                                   correct = FALSE,
                                   conf.level = NA,
                                   method = c("default", "wilson", "exact")) {
  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  f <- parse_formula(formula=formula, data=data, split_chars=c("+", "-"))
  if(f$about$ops[2] != "-" || nrow(f$about)!=2) stop("expected y2 - y1 in formula")
  y2 <- checkif2(f$data$left.1)
  y1 <- checkif2(f$data$left.2)
  if(!all.equal(levels(y2), levels(y1))) stop("levels of both variables must agree")
  if(missing(success)) {
    if(nlevels(y1)==2) {
      success <- levels(y1)[2]
    } else {
      success <- levels(y1)[1]
    }
  }
  stopifnot(success %in% levels(y1))
  m <- table(y1==success, y2==success)
  k <- m[3]
  n <- sum(m[2:3])
  result <- one_proportion_test(k, n, null=0.5, conf.level=conf.level, alternative=alternative, correct=correct, method=method) |>
    mutate(.y_contrast=paste(f$about$var.names, collapse=" - "), .y_value=success)
  result$about[[1]][1] <- paste("McNemar's test, using", result$about[[1]][1])
  as_atest(result)
}

