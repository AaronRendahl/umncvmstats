#' One-sample Wilcoxon test
#'
#' Perform Wilcoxon's signed-rank test on a single sample
#'
#' @param formula a formula of the form `~ y` or `y ~ 1`, where `y` is a numeric variable.
#'     To perform test within subgroups, use `y ~ x` or `y ~ 1 | g`, or even `y ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, a null of a center at 0 is used.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1. Use NA to hide.
#' @param median a logical indicating if the median should be displayed
#' @param pseudomedian a logical indicating if the pseudomedian (and associated confidence interval) should be displayed
#' @param statistic a logical indicating if the V statistic should be displayed
#'
#' @importFrom stats t.test
#' @export
one_wilcoxon_inference <- function(formula, data,
                       alternative = c("two.sided", "less", "greater"),
                       null = 0, conf.level = 0.95,
                       median = TRUE, range=FALSE, pseudomedian = TRUE, statistic = TRUE) {

  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)
  do.pseudomedian <- pseudomedian
  do.median <- median
  do.conf <- pseudomedian & !is.na(conf.level)
  do.test <- !is.na(null)
  do.stat <- statistic
  do.range <- range
  ## need to set defaults in case one is desired without the other
  if(!do.test) null <- 0
  if(!do.conf) conf.level <- 0.95

  f <- parse_formula(formula=formula, data=data)
  x <- f$data$left
  x <- x[!is.na(x)]
  name <- f$about$var.names
  nn <- length(x)
  if(nn==0) {
    result <- tibble(n=0L)
    if(median) result <- result |> mutate(median=NA)
    if(do.pseudomedian) result <- result |> mutate(pseudomedian=NA)
    about <- "No non-missing values found."
  } else if(nn==1) {
    result <- tibble(n=1L)
    if(do.median) result <- result |> mutate(median=x)
    if(do.pseudomedian) result <- result |> mutate(pseudomedian=x)
    about <- "Unable to do inference with only one non-missing value."
  } else {
    w1 <- c("cannot compute exact p-value with ties",
            "cannot compute exact p-value with zeroes",
            "cannot compute exact confidence interval with ties",
            "cannot compute exact confidence interval with zeroes")
    w2 <- c("Approximate p-value used, due to ties or zeros.",
            "Approximate p-value used, due to ties or zeros.",
            "Approximate confidence interval used, due to ties or zeros.",
            "Approximate confidence interval used, due to ties or zeros.")
    ## hide the warnings if not doing the test or CI
    if(!do.conf) {
      w2[c(3:4)] <- NA
    }
    if(!do.test) {
      w2[c(1:2)] <- NA
    }
    ttX <- capture_warnings(
      wilcox.test(x, alternative=alternative, mu=null,
                  conf.level=conf.level, conf.int=do.pseudomedian | do.conf),
      warnings = w1, replacement=w2)
    tt <- ttX$result
    result <- tidy(tt) |> mutate(n=nn)
    about <- sprintf("%s (%s)%s.",
                     result$method, result$alternative,
                     if(do.conf) sprintf(", with %0.0f%% confidence intervals", conf.level*100) else "")
    about <- c(about, unique(ttX$warnings))
    result <- result |> select(-c("method", "alternative"))
    result <- result |> mutate(.y=name, .before=1)
    if(do.test | do.conf) {
      result$about <- list(about)
    } else {
      result$about <- list(character())
    }
    about.vars <- "n"
    est.vars <- NA_character_
    inf.vars <- character()
    if(!do.test) {
      result <- result |> select(-c("statistic", "p.value"))
    } else {
      if(do.pseudomedian) {
        result <- result |> mutate(null=null)
        inf.vars <- c(inf.vars, "null")
      }
      if(do.stat) {
        result <- result |> rename(V="statistic")
        inf.vars <- c(inf.vars, "V")
      } else {
        result <- result |> select(-any_of("statistic"))
      }
    }
    if(do.median) {
      result <- result |> mutate(median=median(x))
      about.vars <- c(about.vars, "median")
    }
    if(do.range) {
      result <- result |> mutate(min=min(x), max=max(x))
      about.vars <- c(about.vars, "min", "max")
    }
    if(do.pseudomedian) {
      result <- result |> rename(pseudomedian="estimate")
      est.vars <- "pseudomedian"
    } else {
      result <- result |> select(-any_of("estimate"))
    }
    if(!do.conf) {
      result <- result |> select(-any_of(c("conf.low", "conf.high")))
    }
    as_atest(result, about.vars=about.vars, estimate.vars=est.vars, inference.vars=inf.vars)
  }
}

#' Two-sample Wilcoxon test
#'
#' Perform the Wilcoxon rank sum to compare two groups
#'
#' @param formula a formula of the form `y ~ x`, where `y` is a numeric variable and `x` is a factor variable.
#'     To perform test within subgroups, use `y ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#' @param conf.adjust adjust confidence bounds for `conf.adjust` simultaneous intervals using the Bonferroni method.
#'   Used internally by `pairwise_wilcox_inference`; should only rarely be used by users.
#' @param pseudomedian a logical indicating if the pseudomedian (and associated confidence interval) should be displayed
#' @param statistic a logical indicating if the W statistic should be displayed
#'
#' @importFrom stats wilcox.test
#' @export
two_wilcoxon_inference <- function(formula, data,
                                   alternative = c("two.sided", "less", "greater"),
                                   null = 0,
                                   conf.level = 0.95, conf.adjust = 1,
                                   pseudomedian = TRUE, statistic=TRUE) {

  a <- test_by()
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)

  do.pseudomedian <- pseudomedian
  do.conf <- pseudomedian & !is.na(conf.level)
  do.stat <- statistic
  if(!do.conf) conf.level <- 0.95

  f <- parse_formula(formula=formula, data=data)

  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  x <- checkif2(x)
  use.conf.level <- 1 - (1-conf.level)/conf.adjust

  w1 <- c("cannot compute exact p-value with ties",
          "cannot compute exact confidence intervals with ties")
  w2 <- c("Approximate p-value used, due to ties.",
          "Approximate confidence interval used, due to ties.")
  if(!do.conf) w2[2] <- NA
  ttX <- capture_warnings(
    wilcox.test(y~x, alternative=alternative, mu=null,
                conf.level=use.conf.level, conf.int=do.pseudomedian | do.conf),
    warnings = w1, replacement=w2)
  tt <- ttX$result
  result <- broom::tidy(tt)
  adjust_txt <- if(conf.adjust!=1) sprintf(", adjusted for %d comparisons using the Bonferroni method", conf.adjust) else ""
  about <- sprintf("%s (%s)%s.",
                   result$method, result$alternative,
                   if(do.conf) sprintf(", with %0.0f%% confidence intervals%s", conf.level*100, adjust_txt) else "")
  about <- c(about, ttX$warnings)
  result <- result |>
    mutate(.x_contrast=paste(levels(x), collapse=" - ")) |>
    rename(W="statistic") |>
    select(-c("method", "alternative")) |>
    mutate(.y = y.name, .x = x.name)
  inf.vars <- character()
  if(do.pseudomedian) {
    result <- result |> rename(pseudomedian="estimate") |> mutate(null=null)
    estname <- "pseudomedian"
    inf.vars <- c(inf.vars, "null")
  } else {
    result <- result |> select(-any_of("estimate"))
    estname <- NA_character_
  }
  if(do.stat) {
    inf.vars <- c(inf.vars, "W")
  } else {
    result <- result |> select(-any_of("W"))
  }
  result$about <- list(about)
  as_atest(result, estimate.vars=estname, inference.vars=inf.vars)
}

#' @param adjust method of adjusting p-values for multiple comparisons, one of "`bonferroni`", "`holm`", or "`none`".
#' @param reverse reverse the direction of pairwise comparisons.
#' @param ... further arguments to be passed to submethods, as appropriate.
#' @rdname two_wilcoxon_inference
#' @export
pairwise_wilcoxon_inference <- function(formula, data, adjust=c("bonferroni", "holm", "none"), reverse=FALSE, ...) {
  pairwise(formula, data, "two_wilcoxon_inference", adjust=adjust, reverse=reverse, ...)
}

#' Paired Wilcoxon-test
#'
#' Compute the Wilcoxon ranked sum between paired samples.
#'
#' @param formula a formula of the form `y2 - y1 ~ 1` or `~ y2 -y1`, where `y1` and `y2` are numeric variables.
#'     To perform test within subgroups, use `y2 - y1 ~ x` or `y2 - y1 ~ 1 | g`, or even `y2- y1 ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#' @param alternative character string specifying the alternative hypothesis, must be one of "`two.sided`" (default), "`greater`" or "`less`".
#' @param null a number specifying the null proportion for testing a null hypothesis; if not specified, no hypothesis test is performed.
#' @param conf.level confidence level of the returned confidence interval. Must be a single number between 0 and 1.
#'
#' @export
#' @importFrom stats wilcox.test
paired_wilcoxon_inference <- function(formula, data,
                          alternative = c("two.sided", "less", "greater"),
                          null = 0,
                          conf.level = 0.95) {
  a <- test_by(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)

  f <- parse_formula(formula=formula, data=data, split_chars=c("+", "-"))
  if(f$about$ops[2] != "-" || nrow(f$about)!=2) stop("expected y2 - y1 in formula")
  y.names <- f$about$var.names
  backtransform <- all(str_detect(y.names, "^log\\(.*\\)$")) && isTRUE(backtransform)
  w1 <- c("cannot compute exact p-value with ties",
          "cannot compute exact confidence intervals with ties")
  w2 <- c("Approximate p-value used, due to ties.",
          "Approximate confidence interval used, due to ties.")
  ttX <- capture_warnings(
    wilcox.test(f$data$left.1, f$data$left.2, paired=TRUE, conf.level=conf.level, conf.int=TRUE),
    warnings = w1, replacement=w2)
  tt <- ttX$result
  result <- tidy(tt) |> mutate(SE=tt$stderr)
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals.",
                   result$method, result$alternative, conf.level*100)
  about <- c(about, ttX$warnings)
  response <- paste(y.names, collapse=" - ")
  result <- result |>
    select(pseudomedian="estimate", "conf.low", "conf.high", V="statistic", "p.value") |>
    mutate(.y_contrast=response, null=null)
  estname <- "pseudomedian"
  result$about <- list(about)
  as_atest(result, estimate.vars=estname, inference.vars=c("null", "V"))
}

#' Kruskal-Wallis test
#'
#' Perform the Kruskal Wallis test to compare two or more groups
#'
#' @param formula a formula of the form `y ~ x`, where `y` is a numeric variable and `x` is a factor variable.
#'     To perform test within subgroups, use `y ~ x | g`,
#'     where `x` and `g` are factor variables.
#' @param data a data frame containing the values in the formula.
#'
#' @importFrom stats kruskal.test
#' @export
kruskal_wallis_test <- function(formula, data) {

  a <- test_by()
  if(!is.null(a)) return(a)

  f <- parse_formula(formula=formula, data=data)

  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  x <- checkif2(x, require_two = FALSE)

  w1 <- c("cannot compute exact p-value with ties",
          "cannot compute exact confidence intervals with ties")
  w2 <- c("Approximate p-value used, due to ties.",
          "Approximate confidence interval used, due to ties.")
  ttX <- capture_warnings(
    kruskal.test(y~x),
    warnings = w1, replacement=w2)
  tt <- ttX$result
  result <- broom::tidy(tt)
  about <- result$method
  about <- c(about, ttX$warnings)
  result <- result |>
    rename(chisq="statistic", df="parameter") |>
    select(-c("method")) |>
    mutate(.y = y.name, .x = x.name)
  result$about <- list(about)
  as_atest(result, inference.vars=c("chisq", "df"))
}
