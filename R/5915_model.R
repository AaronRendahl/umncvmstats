#' Get model coefficients
#'
#' @param model
#'
#' @return XX
#' @export
model_coefs <- function(model) {
  model |> tidy()
}

#' Get model anova table
#'
#' @param model XX
#' @return XX
#' @export
#' @importFrom car Anova
model_anova <- function(model) {
  Anova(model) |> tidy()
}

#' Get model means
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
  out
}

#' Get model means
#'
#' @param model XX
#' @param formula XX
#' @param ... XX
#' @export
#' @importFrom emmeans emmeans
pairwise_model_means <- function(model, formula, ...) {
  em <- emmeans(model, formula, ...)
  pairs(em, infer=TRUE) |> summary()
}

#' Get model slopes
#'
#' @param model XX
#' @param formula XX
#' @param ... XX
#' @export
#' @importFrom emmeans emtrends
model_slopes <- function(model, formula, ...) {
  var <- format(formula[[2]])
  formula[[2]] <- formula[[3]]
  formula[[3]] <- NULL
  emtrends(model, specs=formula, var=var, ...)
}
