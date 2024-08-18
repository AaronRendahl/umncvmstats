#' One-sample proportion test
#'
#' Compute the proportion of "successes" in a sample and the corresponding
#' confidence interval. Optionally, compute a p-value for a
#' specified null hypothesis.
#'
#' By default only the confidence interval for the proportion is reported,
#' a hypothesis test can also be performed by specifying the desired null value.
#'
#' Two methods are currently supposed, either Wilson's method (both with or
#' without a continuity correction) or the Clopper-Pearson "exact" method.
#' If no method is specified, the default method is Wilson's method without
#' continuity correction, however, the "exact" method is chosen if the sample
#' size is less than 10, the observed proportion is less than 0.10, or the
#' minimum expected count under the null is less than 5 (if a null hypothesis
#' is specified).
#'
#' Wilson's method uses [stats::prop.test] and the exact method uses
#' [stats::binom.test].
#'
#' @param formula a formula of the form `~ y` or `y ~ 1`, where `y` is a factor variable.
#'     If not a factor, it will be automatically converted.
#'     To perform test within subgroups, use `y ~ x` or `y ~ 1 | g`, or even `y ~ x | g`.
#' @param data a data frame containing the values in the formula.
#' @param success an optional vector specifying the level(s) for which proportions should be reported.
#' @param all_success if TRUE, then proportions for all levels are reported.
#' @param method character string specifying which method to use. One of "`default`", "`wilson`", or "`exact`".
#' @param correct a logical indicating whether Yates' continuity correction should be applied; used for Wilson test only.
#' @param alternative  character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param x number of successes.
#' @param n number of trials.
#' @param ... additional arguments, currently unused.
#'
#' @return A tibble with class `atest` containing columns as follows:
#' \item{x}{count of successes}
#' \item{n}{sample size}
#' \item{proportion}{proportion of successes}
#' \item{conf.low}{lower confidence bound}
#' \item{conf.high}{upper confidence bound}
#' \item{null}{the specified null value (if specified)}
#' \item{p.value}{the p-value of the test (if null specified)}
#' @rdname one_proportion_test
#' @export
one_proportion_test.formula <- function(formula, data,
                                        success, all_success=FALSE,
                                        method = c("default", "wilson", "exact"),
                                        correct = FALSE,
                                        alternative = c("two.sided", "less", "greater"),
                                        conf.level = 0.95,
                                        null = NULL,
                                        ...) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)

  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  f <- parse_formula(formula=formula, data=data)
  x <- f$data$left
  name <- f$about$var.names
  x <- checkif2(x, require_two=FALSE)
  if(missing(success)) {
    if(!isTRUE(all_success)) {
      if(nlevels(x)==2) {
        success <- levels(x)[2]
      } else {
        success <- levels(x)[1]
      }
    } else {
      success <- levels(x)
    }
  }
  stopifnot(success %in% levels(x))
  x <- x[!is.na(x)]
  n <- length(x)
  map(success, function(lev) {
    k <- sum(x == lev)
    one_proportion_test.default(k, n, method=method, correct=correct, alternative=alternative,
                                conf.level=conf.level, null=null) |>
      mutate(.y=name, .y_value=lev)
  }) |> combine_tests_list()
}

#' @rdname one_proportion_test
#' @export
one_proportion_test.default <- function(x, n,
                                        method = c("default", "wilson", "exact"),
                                        correct = FALSE,
                                        alternative = c("two.sided", "less", "greater"),
                                        conf.level = 0.95,
                                        null = NULL,
                                        ...) {
  method <- match.arg(method)
  alternative <- match.arg(alternative)
  if(length(x)!=1 || x < 0) stop("x must be a single non-negative integer.")
  if(length(n)!=1 || n < 0) stop("n must be a single non-negative integer.")
  if(x > n) stop("x must be less or equal to n")
  txt <- NULL
  if(n==0) {
    return(as_atest(tibble(x=NA_integer_, n=0L, about=list(c("No non-missing values found."))),
                    estimate.vars=c("x", "n", "proportion")))
  }
  if(method == "default") {
    if(!is.null(null) && min(null, 1-null)*n < 5) {
      method <- "exact"; txt <- "expected observed under null < 5"
    }
    if(min(x, n-x)/n < 0.1) {
      method <- "exact"; txt <- "observed proportion < 0.10"
    }
    if(n < 10) {
      method <- "exact"; txt <- "n < 10"
    }
    txt <- sprintf("Method chosen because %s.", txt)
  }
  if(method=="exact") {
    out <- binomial_test(x=x, n=n, null=null, alternative=alternative, conf.level=conf.level)
    out$about[[1]] <- c(out$about[[1]], txt)
    out
  } else {
    wilson_test(x=x, n=n, null=null, alternative=alternative, conf.level=conf.level, correct=correct)
  }
}

#' @rdname one_proportion_test
#' @export
one_proportion_test <- function(x, ...) { UseMethod("one_proportion_test") }

#' @rdname one_proportion_test
#' @importFrom stats prop.test
#' @export
wilson_test <- function(x, n, null,
                            alternative = c("two.sided", "less", "greater"),
                            conf.level = 0.95, correct = FALSE) {
  alternative <- match.arg(alternative)
  do.test <- !missing(null) && !is.null(null)
  if(!do.test) { null <- 0.5 }
  do.ci <- !is.na(conf.level)
  if(!do.ci) { conf.level <- 0.95 }
  x <- as.integer(x)
  n <- as.integer(n)
  result <- prop.test(x = x, n = n, p = null, alternative=alternative,
                      conf.level=conf.level, correct=correct) |> broom::tidy()
  correct_txt <- if(correct) ", with continuity adjustment" else ""
  ci_txt <- if(do.ci) sprintf(", with %0.0f%% confidence intervals", conf.level*100) else ""
  about <- sprintf("Wilson's proportion test (%s%s)%s.",
                   result$alternative, correct_txt, ci_txt)
  result <- result |> select(-c("method", "alternative", "parameter")) |>
    mutate(x=x, n=n, .before="estimate") |>
    rename(proportion="estimate")
  if(do.test) {
    result <- result |> mutate(null=null, .before="p.value") |>
      rename(chisq.value="statistic") |>
      relocate(any_of(c("null", "chisq.value", "p.value")), .after="conf.high")
  } else {
    result <- result |> select(-c("statistic", "p.value"))
  }
  if(!do.ci) {
    result <- result |> select(-c("conf.low", "conf.high"))
  }
  result$about <- list(about)
  result$SE <- sqrt((x/n)*(1-x/n)/n)
  as_atest(result,
           estimate.vars=c("x", "n", "proportion"),
           inference.vars=c("null", "chisq.value"))
}

#' @rdname one_proportion_test
#' @importFrom stats binom.test
#' @export
binomial_test <- function(x, n, null,
                          alternative = c("two.sided", "less", "greater"),
                          conf.level = 0.95) {
  alternative <- match.arg(alternative)
  do.test <- !missing(null) && !is.null(null)
  if(!do.test) { null <- 0.5 }
  do.ci <- !is.na(conf.level)
  if(!do.ci) { conf.level <- 0.95 }
  x <- as.integer(x)
  n <- as.integer(n)
  result <- binom.test(x = x, n = n, p=null, alternative=alternative,
                      conf.level=conf.level) |> broom::tidy()
  ci_txt <- if(do.ci) sprintf(", with %0.0f%% confidence intervals", conf.level*100) else ""
  about <- sprintf("%s (%s)%s.",
                   result$method, result$alternative, ci_txt)
  result <- result |> select(-c("method", "alternative", "parameter", "statistic")) |>
    mutate(x=x, n=n, .before="estimate") |>
    rename(proportion="estimate")
  if(do.test) {
    result <- result |> mutate(null=null, .before="p.value") |>
      relocate(any_of(c("null", "p.value")), .after="conf.high")
  } else {
    result <- result |> select(-"p.value")
  }
  if(!do.ci) {
    result <- result |> select(-c("conf.low", "conf.high"))
  }
  result$about <- list(about)
  result$SE <- sqrt((x/n)*(1-x/n)/n)
  as_atest(result,
           estimate.vars=c("x", "n", "proportion"),
           inference.vars=c("null", "chisq.value"))
}
