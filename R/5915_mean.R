#' @export
one_t_test <- function(formula, data,
                       alternative = c("two.sided", "less", "greater"),
                       null, conf.level = 0.95,
                       backtransform=TRUE) {

  a <- do_subformulas(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)
  do.test <- !missing(null)
  if(!do.test) { null <- 0 }

  f <- parse_formula(formula=formula, data=data)
  x <- f$data$left
  name <- f$about$var.names
  result <- t.test(x, alternative=alternative, mu=null,
                   var.equal=var.equal, conf.level=conf.level) |>
    broom::tidy()
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals.",
                   result$method, result$alternative, conf.level*100)
  result <- result |> select(-method, -alternative) |>
    rename(mean=estimate) |>
    mutate(response=name, .before=1)
  if(!do.test) {
    result <- result |> select(-statistic, -parameter, -p.value)
  } else {
    result <- result |> mutate(null=null) |>
      rename(t.value=statistic, df=parameter) |>
      relocate(null, t.value, df, p.value, .after=conf.high)
  }
  result$about <- list(about)
  if(str_detect(result$response, "^log\\(.*\\)$") && isTRUE(backtransform)) {
    result$response <- str_replace(result$response, "^log\\((.*)\\)$", "\\1")
    result <- result |> mutate(across(any_of(c("mean","conf.low", "conf.high", "null")), exp))
    result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the geometric mean is reported).")
  }
  as_atest(result)
}

#' @export
two_t_test <- function(formula, data,
                    alternative = c("two.sided", "less", "greater"),
                    null = 0,
                    var.equal = FALSE,
                    conf.level = 0.95, conf.adjust = 1,
                    backtransform = TRUE) {

  a <- do_subformulas()
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
  result <- t.test(y~x, alternative=alternative, mu=null,
                   var.equal=var.equal, conf.level=use.conf.level) |>
    broom::tidy()
  adjust_txt <- if(conf.adjust!=1) sprintf(", adjusted for %d comparisons using the Bonferroni method", conf.adjust) else ""
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals%s.",
                   result$method, result$alternative, conf.level*100, adjust_txt)
  result <- result |>
    mutate(value=paste(levels(x), collapse=if(backtransform) " / " else " - "), .after=estimate2) |>
    rename(mean="estimate", df=parameter, t.value=statistic) |>
    select(-estimate1, -estimate2, -method, -alternative) |>
    mutate(null=null) |>
    relocate(null, t.value, df, p.value, .after="conf.high") |>
    mutate(response = y.name, variable = x.name, .before=1)
  result$about <- list(about)
  if(backtransform) {
     result$response <- str_replace(result$response, "^log\\((.*)\\)$", "\\1")
     result <- result |> mutate(across(any_of(c("mean","conf.low", "conf.high", "null")), exp))
     result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the ratio is reported).")
  }
  as_atest(result)
}

#' @export
paired_t_test <- function(formula, data,
                          alternative = c("two.sided", "less", "greater"),
                          null = 0,
                          conf.level = 0.95,
                          backtransform = TRUE) {
  a <- do_subformulas(by_right=TRUE)
  if(!is.null(a)) return(a)

  alternative <- match.arg(alternative)

  f <- parse_formula(formula=formula, data=data, split_chars=c("+", "-"))
  if(f$about$ops[2] != "-" || nrow(f$about)!=2) stop("expected y2 - y1 in formula")
  y.names <- f$about$var.names
  backtransform <- all(str_detect(y.names, "^log\\(.*\\)$")) && isTRUE(backtransform)

  result <- t.test(f$data$left.1, f$data$left.2, paired=TRUE) |> tidy()
  about <- sprintf("%s (%s), with %0.0f%% confidence intervals.",
                   result$method, result$alternative, conf.level*100)
  if(backtransform) y.names <- str_replace(y.names, "^log\\((.*)\\)$", "\\1")
  response <- paste(y.names, collapse=if(backtransform) " / " else " - ")
  result <- result |> mutate(response=response, null=null) |>
    select(response, mean=estimate, conf.low, conf.high, null, t.value=statistic, df=parameter, p.value)
  result$about <- list(about)
  if(backtransform) {
    result <- result |> mutate(across(any_of(c("mean","conf.low", "conf.high", "null")), exp))
    result$about[[1]] <- c(result$about[[1]], "Results are backtransformed from the log scale (that is, the ratio is reported).")
  }
  as_atest(result)
}
