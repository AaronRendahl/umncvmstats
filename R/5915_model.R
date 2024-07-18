model_form <- function(model) {
  f <- model$terms
  tibble(.y=format(f[[2]]), .terms=format(f[[3]]))
}

#' Get model summary information
#'
#' @param model XX
#' @param ... XX
#'
#' @return XX
#' @importFrom broom glance
#' @export
model_glance <- function(model, ...) {
  out <- glance(model, ...)
  bind_cols(model_form(model), out) |> as_atest(inference.vars=setdiff(names(out), "p.value"))
}

#' Get model coefficients
#'
#' @param model XX
#' @param ... XX
#'
#' @return XX
#' @export
model_coefs <- function(model, ...) {
  out <- tidy(model, ...) |> rename(any_of(c(SE="std.error")))
  bind_cols(model_form(model), out) |>
    as_atest(estimate.vars=c("term", "estimate", "SE"),
             inference.vars=c("statistic"))
}

#' Get model anova table
#'
#' @param model XX
#' @param ... XX
#' @return XX
#' @export
#' @importFrom car Anova
model_anova <- function(model, ...) {
  out <- Anova(model, ...) |> tidy()
  bind_cols(model_form(model), out) |> as_atest(inference.vars=setdiff(names(out), "p.value"))
}

#' Get model means and slopes (trends)
#'
#' @param model XX
#' @param formula XX
#' @param ... XX
#' @param cld XX
#' @return XX
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
  } else {
    out <- summary(em)
  }

  attr(out, "mesg") <- c(attr(out, "mesg"), emX$warnings)
  as_atest(out, model,
           estimate.vars=c("emmean", "response", "estimate", "ratio", "SE", "df"),
           inference.vars=c("null", "t.ratio"))
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
  as_atest(out, model,
           pri.vars="contrast",
           estimate.vars=c("emmean", "response", "estimate", "ratio", "SE", "df"),
           inference.vars="null", "t.ratio")
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
  } else {
    out <- summary(em)
  }
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
