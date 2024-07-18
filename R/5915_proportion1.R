
#' One proportion test
#'
#' A short description XX more ...
#'
#' More details
#'
#' @param formula a formula of the form `~ x` or `x ~ 1`, where `x` is a factor variable.
#'     If not a factor, it will be automatically converted.
#' @param data a data frame containing the values in the formula
#' @param x number of successes
#' @param n number of trials
#' @param success optional: the level(s) for which proportions should be reported
#' @param all_success if TRUE, then proportions for all levels are reported
#' @param null null proportion (optional)
#' @param alternative  character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param correct a logical indicating whether Yates' continuity correction should be applied where possible.
#' @param method character string specifying which method to use. One of "default", "wilson", or "exact".
#' @param ... more things
#'
#' @return An atest
#' @export
one_proportion_test <- function(x, ...) { UseMethod("one_proportion_test") }

#' @rdname one_proportion_test
#' @export
one_proportion_test.formula <- function(formula, data, success, all_success=FALSE, ...) {

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
    one_proportion_test.default(k, n, ...) |>
      mutate(.y=name, .y_value=lev)
  }) |> combine_tests_list()
}

#' @rdname one_proportion_test
#' @export
one_proportion_test.default <- function(x, n, null,
                            alternative = c("two.sided", "less", "greater"),
                            conf.level = 0.95, correct = FALSE,
                            method = c("default", "wilson", "exact"),
                            ...) {
  method <- match.arg(method)
  if(length(x)!=1) stop("x must be a single integer.")
  if(length(n)!=1) stop("x must be a single integer.")
  txt <- NULL
  if(method == "default") {
    if(!missing(null) && min(null, 1-null)*n < 5) {
      method <- "exact"; txt <- "expected observed under null < 5"
    }
    if(x/n < 0.1) {
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
#' @importFrom stats prop.test
#' @export
wilson_test <- function(x, n, null,
                            alternative = c("two.sided", "less", "greater"),
                            conf.level = 0.95, correct = FALSE) {
  alternative <- match.arg(alternative)
  do.test <- !missing(null)
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
  do.test <- !missing(null)
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
  as_atest(result,
           estimate.vars=c("x", "n", "proportion"),
           inference.vars=c("null", "chisq.value"))
}
