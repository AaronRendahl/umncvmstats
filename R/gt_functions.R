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
