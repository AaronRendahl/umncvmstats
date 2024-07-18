#' Create a gt table object
#' @param data XX
#' @param ... XX
#'
#' @export
as_gt <- function(data, ...) { UseMethod("as_gt") }

#' @export
#' @rdname as_gt
as_gt.default <- function(data, ...) {
  if(inherits(data, "gt_tbl")) {
    data
  } else {
    gt::gt(data, ...)
  }
}

#' @param footnote_col XX
#' @param rowname_col XX
#' @param groupname_col XX
#' @param simplify XX
#' @param row_group_as_column XX
#' @rdname as_gt
#' @export
as_gt.atest <- function(data,
                        footnote_col="footnote",
                        rowname_col=c(),
                        groupname_col=c(),
                        simplify = TRUE,
                        row_group_as_column = TRUE,
                        ...) {
  xx <- separate_about(data)
  d <- xx$result
  a <- xx$about
  if(isTRUE(simplify)) d <- simplify_atest(d)

  title <- attr(d, "title")
  em.groups <- attr(d, "by.vars")
  if(length(groupname_col) > 0) {
    # leave it alone...
  } else if(".GROUP" %in% names(d)) {
    groupname_col <- ".GROUP"
  } else if(!is.null(em.groups)) {
    groupname_col <- em.groups
    for(g in groupname_col) { d[[g]] <- paste(g, d[[g]], sep=" = ") }
  } else {
    groupname_col <- dplyr::group_vars(d)
    for(g in groupname_col) { d[[g]] <- paste(g, d[[g]], sep=" = ") }
  }
  d <- ungroup(d)

  if(!is.null(a)) {
    if(any(!is.na(a$.row)) && !footnote_col %in% names(d))
      d[[footnote_col]] <- ""
  }

  out <- d |> select(-any_of(".row")) |>
    gt(groupname_col=groupname_col, rowname_col=rowname_col,
       row_group_as_column=row_group_as_column,
       row_group.sep=", ", ...) |>
    fmt_numbers(n_sigfig = 2) |>
    cols_align_decimal(where(is.double) & !any_of(c("p.value", "p.adjust"))) |>
    fmt_pvalue() |>
    sub_missing(missing_text="") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_options(table.align='left')

  if(!is.null(title)) {
    out <- out |> tab_header(title=title)
  }
  if(!is.null(a)) {
    out <- out |> tab_footnotes(a$footnote.text, footnote_col, a$.row)
  }
  out
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
