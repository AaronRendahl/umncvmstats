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
#' @examples
#' mtcars2 |> dplyr::select(-model) |> descriptive_statistics()
#'
#' @export
#'
#' @importFrom gtsummary tbl_summary
#' @importFrom gtsummary as_gt
#' @importFrom gtsummary all_continuous
#' @importFrom gtsummary all_categorical
#' @importFrom gtsummary with_gtsummary_theme
#' @importFrom gtsummary add_stat_label
descriptive_statistics <- function(data, ..., digits=2, compact=TRUE) {
  theme_here <- list(
    "tbl_summary-arg:digits" = all_continuous() ~ digits,
    "tbl_summary-str:default_con_type" = "continuous2",
    "tbl_summary-arg:statistic" = list(all_continuous() ~ c("{mean} \u00B1 {sd}", "{median} ({minX}, {maxX})"),
                                       all_categorical() ~ "{n}/{N} ({p}%)"))
  out <- with_gtsummary_theme(theme_here, tbl_summary(data, ...)) |>
    add_stat_label(label=all_continuous() ~ c("Mean \u00B1 SD", "Median (Min, Max)")) |>
    as_gt()
  if(compact) {
    out <- out |> tab_compact()
  }
  out
}

minX <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if(n<=1) NA else min(x)
}

maxX <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if(n<=1) NA else max(x)
}
