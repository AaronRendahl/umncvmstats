#' Compact a gt table
#'
#' Compact a gt table. Uses `tab_options` to set `table.font.size` and
#' the various padding options (see Details).
#'
#' Padding options that are set are `data_row.padding`, `summary_row.padding`,
#' `grand_summary_row.padding`, `footnotes.padding`, `source_notes.padding`, and
#' `row_group.padding`; all are set to the same value.
#'
#' @param x the table to compact.
#' @param font.size the desired font size.
#' @param padding spacing between rows, in pixels.
#'
#' @export
#' @importFrom gt tab_options
tab_compact <- function(x, font.size=13, padding=1) {
  x |> tab_options(table.font.size = font.size) |>
    tab_padding(padding=padding)
}

#' @importFrom gt px
#' @importFrom gt tab_options
#' @rdname tab_compact
#' @export
tab_padding <- function(x, padding) {
  sp <- px(padding)
  x |> tab_options(
    data_row.padding = sp,
    summary_row.padding = sp,
    grand_summary_row.padding = sp,
    footnotes.padding = sp,
    source_notes.padding = sp,
    row_group.padding = sp)
}


tab_footnotes <- function(data, notes, columns=NA, rows=NA) {
  if(missing(notes) || is.null(notes)) return(data)
  aa <- tibble(note=notes, columns=columns, rows=rows) |>
    left_join(data$`_boxhead` |> select(columns="var", "type"), by="columns")
  hidden_cols <- aa |> filter(!is.na(.data$rows) & (is.na(.data$type) | .data$type=="hidden"))
  if(nrow(hidden_cols)>0) {
    hidden_txt <- hidden_cols |>
      pull("columns") |>
      (\(x) sprintf("'%s'", x))() |>
      (\(x) paste(x, collapse=", "))()
    warning(sprintf("footnote column %s is missing or hidden", hidden_txt))
  }
  for(idx in seq_len(nrow(aa))) {
    r <- aa$rows[idx]
    if(is.na(r)) {
      data <- data |> tab_footnote(aa$note[idx])
    } else {
      if(aa$type[idx]=="stub") {
        data <- data |> tab_footnote(aa$note[idx], cells_stub(r))
      } else {
        data <- data |> tab_footnote(aa$note[idx], cells_body(aa$columns[idx], r))
      }
    }
  }
  data
}
