model_form <- function(model) {
  f <- model$terms
  tibble(response=format(f[[2]]), model.terms=format(f[[3]]))
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
  bind_cols(model_form(model), out) |> as_atest()
}

#' Get model coefficients
#'
#' @param model XX
#' @param ... XX
#'
#' @return XX
#' @export
model_coefs <- function(model, ...) {
  out <- tidy(model, ...)
  bind_cols(model_form(model), out) |> as_atest()
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
  bind_cols(model_form(model), out) |> as_atest()
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
model_means <- function(model, formula, ..., cld=TRUE) {
  em <- emmeans(model, formula, ...)
  if(isTRUE(cld)) {
    out <- cld(em, Letters=letters)
  } else {
    out <- summary(em)
  }
  as_atest(out, model)
}

#' @export
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @rdname model_means
pairwise_model_means <- function(model, formula, ...) {
  em <- emmeans(model, formula, ...)
  out <- pairs(em, infer=TRUE) |> summary()
  as_atest(out, model)
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
    out <- cld(em, Letters=letters)
  } else {
    out <- summary(em)
  }
  as_atest(out, model)
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
  as_atest(out, model)
}
