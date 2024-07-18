#' Summarize variables
#'
#' @param data XX
#' @param ... XX
#' @param digits XX
#' @param compact XX
#'
#' @return XX
#' @export
#'
#' @importFrom gtsummary tbl_summary
#' @importFrom gtsummary as_gt
#' @importFrom gtsummary all_continuous
#' @importFrom gtsummary with_gtsummary_theme
summarize_variables <- function(data, ..., digits=2, compact=TRUE) {
  theme_here <- list(
    "tbl_summary-arg:digits" = all_continuous() ~ digits,
    "tbl_summary-str:categorical_stat" = "{n}/{N} ({p}%)",
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-str:continuous_stat" = c("{mean} \u00B1 {sd}", "{median} ({min}, {max})"))
  out <- with_gtsummary_theme(theme_here, tbl_summary(data, ...)) |> as_gt()
  if(compact) {
    out <- out |> tab_compact()
  }
  out
}
