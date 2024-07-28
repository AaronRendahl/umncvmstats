#' Independence Tests for Count Data
#'
#' Perform an independence test between two categorical variables, using either
#' the chi-squared method or Fisher's exact method.
#'
#' By default, the chi-squared method with no continuity correction is used
#' unless the minimum expected count under the null is less than 5, in which case
#' Fisher's exact test is used.
#'
#' @param formula a formula of the form `y ~ x`, where `y` and `x` are both factor variables.
#'     If not factors, they will be automatically converted.
#'     To perform test within subgroups, use `y ~ x | g`.
#' @param data a data frame containing the values in the formula.
#' @param method character string specifying which method to use. One of "`default`", "`chisq`", or "`exact`".
#' @param correct a logical indicating whether Yates' continuity correction should be applied; used for chi-squared test only.
#' @param ... additional parameters, currently unused.
#'
#' @rdname independence_test
#' @export
independence_test.formula <- function(formula, data, method=c("default", "chisq", "exact"), correct=FALSE, ...) {

  a <- test_by()
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
  independence_test.default(m, method=method, correct=correct) |>
    mutate(.y = y.name, .x = x.name) |>
    as_atest()
}

#' @param x instead of a formula, provide the matrix of counts directly.
#' @importFrom stats chisq.test
#' @rdname independence_test
#' @export
independence_test.default <- function(x, method=c("default", "chisq", "exact"), correct=FALSE, ...) {
  method <- match.arg(method)
  E <- outer(rowSums(x), colSums(x))/sum(x)
  if((method=="default" && any(E < 5)) || method=="exact") {
    ft <- fisher.test(x) |> tidy()
    about <- ft$method
    if(method!="exact") {
      about <- c(about, "Method chosen due to expected counts < 5.")
    }
    result <- ft |> select("p.value")
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
    result <- result |> select(chisq.value="statistic", df="parameter", "p.value", "about")
  }
  as_atest(result, inference.vars=c("chisq.value", "df"))
}

#' @rdname independence_test
#' @export
independence_test <- function(x, ...) { UseMethod("independence_test") }
