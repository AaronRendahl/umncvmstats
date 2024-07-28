#' Compute descriptive statistics for a data set
#'
#' Use `gtsummary::tbl_summary` to summarize variables in a data set.
#' For categorical variables, reports count/total and percent; for
#' continuous variables, reports mean, standard deviation, median, and range.
#'
#' @param data the data set to summarize.
#' @param ... additional parameters, sent to `tbl_summary`.
#' @param digits desired number of significant figures, for numeric variables.
#' @param compact logical specifying whether or not to compact the resulting `gt` table.
#'
#' @export
#'
#' @importFrom gtsummary tbl_summary
#' @importFrom gtsummary as_gt
#' @importFrom gtsummary all_continuous
#' @importFrom gtsummary with_gtsummary_theme
descriptive_statistics <- function(data, ..., digits=2, compact=TRUE) {
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
