#' Independence Tests for Count Data
#'
#' @param x XX
#' @param ... XX
#'
#' @export
independence_test <- function(x, ...) { UseMethod("independence_test") }

#' @param formula XX
#' @param data XX
#' @param ... XX
#'
#' @rdname independence_test
#' @export
independence_test.formula <- function(formula, data, ...) {

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
  independence_test.default(m, ...) |>
    mutate(.y = y.name, .x = x.name) |>
    as_atest()
}

#' @param x XX
#'
#' @param method XX
#' @param correct XX
#' @param ... XX
#'
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
