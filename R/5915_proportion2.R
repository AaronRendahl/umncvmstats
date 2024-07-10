#' @export
two_proportion_test <- function(object, ...) { UseMethod("two_proportion_test") }

#' @export
two_proportion_test.formula <- function(formula, data, success, method=c("default", "chisq", "exact"),
                                        alternative = c("two.sided", "less", "greater"), ...) {

  a <- do_subformulas()
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
  result <- result |> mutate(response = y.name, response.value=success, .before=1) |>
    mutate(variable = x.name,
           value=paste(levels(x), collapse=" - "),
           .before="proportion")
  as_atest(result)
}

#' @export
two_proportion_test.default <- function(x, n,
                                        method=c("default", "chisq", "exact"),
                                        alternative = c("two.sided", "less", "greater"),
                                        conf.level = 0.95, conf.adjust=1, correct = TRUE) {
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
    result <- ft |> select(p.value) |> mutate(proportion=computed.diff, .before=p.value) |>
      mutate(value="difference", .before=1)
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
      mutate(value="difference", .after=estimate2) |>
      mutate(proportion=estimate2-estimate1, .after=value) |>
      select(-method, -alternative, -parameter, -estimate1, -estimate2) |>
      rename(chisq.value=statistic) |>
      relocate(c(chisq.value, p.value), .after=conf.high)
  }
  result$about <- list(about)
  as_atest(result)
}

#' @export
paired_proportion_test <- function(formula, data, success,
                                   alternative = c("two.sided", "less", "greater"),
                                   correct = FALSE,
                                   conf.level = NA, conf.adjust = 1,
                                   method = c("default", "wilson", "exact")) {
  a <- do_subformulas(by_right=TRUE)
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
    mutate(response=paste(f$about$var.names, collapse=" - "), response.value=success)
  result$about[[1]][1] <- paste("McNemar's test, using", result$about[[1]][1])
  as_atest(result)
}

#' @export
independence_test <- function(object, ...) { UseMethod("independence_test") }

#' @export
independence_test.formula <- function(formula, data, ...) {

  a <- do_subformulas()
  if(!is.null(a)) return(a)

  f <- parse_formula(formula=formula, data=data)
  if(!formula_has(f,1,1,0)) stop("improper formula; expecting y~x")
  y <- f$data$left
  x <- f$data$right
  y.name <- f$about$var.names[f$about$side=="left"]
  x.name <- f$about$var.names[f$about$side=="right"]
  y <- checkif2(y, require_two = FALSE)
  x <- checkif2(x, require_two = FALSE)
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  m <- table(x, y)
  independence_test.default(m, ...) |>
    mutate(response=y.name, variable = x.name, .before=1)
}

#' @export
independence_test.default <- function(x, method=c("default", "chisq", "exact"), correct=FALSE) {
  method <- match.arg(method)
  E <- outer(rowSums(x), colSums(x))/sum(x)
  if((method=="default" && any(E < 5)) || method=="exact") {
    ft <- fisher.test(x) |> tidy()
    about <- ft$method
    if(method!="exact") {
      about <- c(about, "Method chosen due to expected counts < 5.")
    }
    result <- ft |> select(p.value)
    result$about <- list(about)
  } else {
    capture <- capture_warnings(
      chisq.test(x, correct=correct),
      "Chi-squared approximation may be incorrect",
      "At least one expected count < 5; chi-squared approximation may be incorrect.")
    result <- capture$result |> tidy()
    if(!is.null(result$statistic) && is.nan(result$statistic)) result$statistic <- NA
    if(!is.null(result$p.value) && is.nan(result$p.value)) result$p.value <- 1
    result$about <- list(c(result$method, capture$warnings))
    result <- result |> select(chisq.value=statistic, df=parameter, p.value, about)
  }
  as_atest(result)
}
