#' Set gt table padding
#'
#' @param x gt table
#' @param table.spacing spacing, in pixels
#'
#' @return a gt.table with modified spacing
#' @export
#' @importFrom gt px
#' @importFrom gt tab_options
tab_padding <- function(x, table.spacing) {
  sp <- px(table.spacing)
  x |> tab_options(
    data_row.padding = sp,
    summary_row.padding = sp,
    grand_summary_row.padding = sp,
    footnotes.padding = sp,
    source_notes.padding = sp,
    row_group.padding = sp)
}

#' Compact a gt table
#'
#' @param x XX
#' @param font.size XX
#' @param spacing XX
#'
#' @return XX
#' @export
#' @importFrom gt tab_options
tab_compact <- function(x, font.size=13, spacing=1) {
  x |> tab_options(table.font.size = font.size) |>
    tab_padding(table.spacing=spacing)
}

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
