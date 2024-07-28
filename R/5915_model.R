model_form <- function(model) {
  f <- model$terms
  .y <- format(f[[2]])
  .y_value <- NULL
  if((.y %in% names(model$data)) &&
     is.factor(model$data[[.y]]) &&
     (nlevels(model$data[[.y]])==2)) {
    .y_value <- levels(model$data[[.y]])[2]
  }
  tibble(.y=.y, .y_value=.y_value, .terms=format(f[[3]]))
}

#' Get model summary information
#'
#' Retrieve statistics such as R-squared, AIC, and more, as computed by `broom::glance`.
#'
#' @param model a linear model or generalized linear model.
#' @param ... additional parameters, passed to `broom::glance`.
#'
#' @importFrom broom glance
#' @export
model_glance <- function(model, ...) {
  out <- glance(model, ...)
  bind_cols(model_form(model), out) |> as_atest(inference.vars=setdiff(names(out), "p.value"))
}

#' Get model coefficients
#'
#' Retrieve coefficients and corresponding confidence intervals and p-values,
#' as computed by `broom::tidy`.
#'
#' @param model a linear model or generalized linear model.
#' @param ... additional parameters, passed to `broom::tidy`.
#'
#' @importFrom broom tidy
#' @export
model_coefs <- function(model, ...) {
  out <- tidy(model, ...) |> rename(any_of(c(SE="std.error")))
  bind_cols(model_form(model), out) |>
    as_atest(estimate.vars=c("term", "estimate", "SE"),
             inference.vars=c("statistic"))
}

#' Get model Anova table
#'
#' Retrieve Anova (analysis of variance) table for a linear model, or analysis
#' of deviance table for a generalized linear model, as computed by `car::Anova`.

#' @param model a linear model or generalized linear model.
#' @param ... additional parameters, passed to `car::Anova`.
#' @export
#' @importFrom car Anova
model_anova <- function(model, ...) {
  out <- Anova(model, ...) |> tidy()
  bind_cols(model_form(model), out) |> as_atest(inference.vars=setdiff(names(out), "p.value"))
}

#' Get model means and slopes (trends)
#'
#' Retrieve estimated marginal means and slopes (trends) as computed by
#' `emmeans::emmeans` and `emmeans::emtrends`, and tests for all pairwise differences,
#' as computed by `emmeans::pairs.emmGrid`.
#'
#' For means, specify the desired combinations of explanatory variables
#' using formula notation. For example, to get the means at all levels of a
#' variable `x`, use `~ x`. To get the means at all combinations of `x1` and `x2`,
#' use `~ x1 + x2.`
#'
#' For slopes, specify the desired slope on the left side of the formula. For
#' example, to get the slopes for a variable `x` at each level of a variable `g`,
#' use `x ~ g.` To get the overall slope, use `x ~ 1`.
#'
#' Additionally, one can specify groupings by using a `|` within a formula.
#' For example, to get the means of all combinations of `x1` and `x2` grouped by each
#' value of `x2`, use `~ x1 | x2`. This is especially useful for getting pairwise
#' tests of means and trends within subgroups.
#'
#' By default, estimated marginal means and trends are estimated by computing
#' the average mean or trend across all values of any other categorical
#' explanatory variables equally, and at the mean value of any other numeric explanatory
#' variables.
#'
#' One of most useful additional parameters that can be passed to `emmeans` is `at`,
#' which allows one to specify specific values of continuous variables to compute
#' means and/or trends at. For example, if one had a continuous variable `x` in the model,
#' one could compute the means at `x=10` and `x=20` using `at = list(x = c(10, 20)`.
#'
#' @param model a linear model or generalized linear model.
#' @param formula the desired means or slopes; see Details.
#' @param cld a logical variable specifying if a compact letter display should be used
#'     for pairwise comparisons between groups.
#' @param backtransform if a linear model and response variable is of form `log(y)`,
#'      or the model is a logistic model , backtransform the resulting estimate and
#'      confidence interval bounds, so that they report either geometric means on the
#'      original scale and ratios (for log-transformed responses) or proportions
#'      and odds ratios (for logistic models).
#' @param type specify method of backtransformation to `emmeans`. If used, `backtransform` is ignored.
#' @param ... additional parameters, passed to `emmeans` or `emtrends`. See Details.
#' @export
#' @importFrom car Anova
#' @importFrom multcomp cld
#' @importFrom emmeans emmeans
model_means <- function(model, formula, cld=TRUE, backtransform=TRUE,
                        type=if(isTRUE(backtransform)) "response" else "linear", ...) {

  int_warn <- "NOTE: Results may be misleading due to involvement in interactions\n"
  emX <- capture_warnings(emmeans(model, formula, type=type, ...))
  em <- emX$result

  if(isTRUE(cld)) {
    out <- cld(em, Letters=letters) |> rename(cld.group=".group")
    skip <- "NOTE: If two or more means share the same grouping symbol,\n      then we cannot show them to be different.\n      But we also did not show them to be the same."
    attr(out, "mesg") <- setdiff(attr(out, "mesg"), skip)
    out[["cld.group"]] <- str_replace_all(out[["cld.group"]], " ", "\u2007")
  } else {
    out <- summary(em)
  }
  if("df" %in% names(out) && all(is.infinite(out[["df"]]))) { out[["df"]] <- NULL }
  attr(out, "mesg") <- c(attr(out, "mesg"), emX$warnings)
  as_atest(out, model,
           estimate.vars=c("emmean", "response", "prob", "estimate", "ratio", "SE", "df"),
           inference.vars=c("null", "t.ratio", "z.ratio"))
}

#' @export
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @rdname model_means
pairwise_model_means <- function(model, formula, backtransform=TRUE,
                                 type=if(isTRUE(backtransform)) "response" else "linear", ...) {
  emX <- capture_warnings({
    em <- emmeans(model, formula, type=type, ...)
    pairs(em, infer=TRUE) |> summary()
  })
  out <- emX$result
  attr(out, "mesg") <- c(attr(out, "mesg"), emX$warnings)
  if("df" %in% names(out) && all(is.infinite(out[["df"]]))) { out[["df"]] <- NULL }
  as_atest(out, model,
           pri.vars="contrast",
           estimate.vars=c("emmean", "response", "odds.ratio", "estimate", "ratio", "SE", "df"),
           inference.vars=c("null", "t.ratio", "z.ratio"))
}

#' @export
#' @importFrom emmeans emtrends
#' @rdname model_means
model_slopes <- function(model, formula, ..., cld=TRUE) {
  var <- format(formula[[2]])
  formula[[2]] <- formula[[3]]
  formula[[3]] <- NULL
  em <- emtrends(model, specs=formula, var=var, ...)
  if(isTRUE(cld)) {
    out <- cld(em, Letters=letters) |> rename(cld.group=".group")
    skip <- "NOTE: If two or more means share the same grouping symbol,\n      then we cannot show them to be different.\n      But we also did not show them to be the same."
    attr(out, "mesg") <- setdiff(attr(out, "mesg"), skip)
    out[["cld.group"]] <- str_replace_all(out[["cld.group"]], " ", "\u2007")
  } else {
    out <- summary(em)
  }
  if("df" %in% names(out) && all(is.infinite(out[["df"]]))) { out[["df"]] <- NULL }
  as_atest(out, model, estimate.vars=setdiff(names(out), c("cld.group", "p.value")))
}

#' @export
#' @importFrom emmeans emtrends
#' @importFrom graphics pairs
#' @rdname model_means
pairwise_model_slopes <- function(model, formula, ...) {
  var <- format(formula[[2]])
  formula[[2]] <- formula[[3]]
  formula[[3]] <- NULL
  em <- emtrends(model, specs=formula, var=var, ...)
  out <- pairs(em, infer=TRUE) |> summary()
  as_atest(out, model, estimate.vars=setdiff(names(out), "p.value"))
}
