#' @export
as_gt.default <- function(x, ...) {
  as.data.frame(x) |> as_gt()
}

#' @export
as_gt.gt_tbl <- function(x, ...) {
  x
}

#' @export
as_gt.gtsummary <- function(x, ...) {
  gtsummary::as_gt(x, ...)
}

#' Create a gt table object
#'
#' Convert an object to a gt table.
#'
#' Behind the scenes, an `atest` object has a number of columns specifying the variables used in the
#' original formula; these include not only the names of the variables (`.y ~ .x | .g`)
#' but also for categorical variables, the values of the variables (`.y_value`, `.x_value`,
#' `.g_value`) and any contrasts (`.y_contrast`, `.x_contrast`) for two-sample, pairwise,
#' or paired comparisons. There is also a `.terms` variable for the right hand side of a
#' an `atest` about a model.
#'
#' When `simplify = TRUE` (the default), these are converted to more readable columns,
#' in particular, any values or contrasts are combined with the variable names, and
#' new variables of `response`, `variable`, `group`, and `model` are used instead of
#' the variables described above.
#'
#' @param x the object to convert.
#' @param footnote_col the column to add footnotes to.
#' @param rowname_col column to use as row names, if desired.
#' @param groupname_col column to use as groups, if desired.
#' @param simplify a logical variable, whether or not to simplify the table first. See Details.
#' @param row_group_as_column a logical variable, whether or not to
#' @param ... additional parameters, passed to `gt`.
#' @rdname as_gt
#' @export
as_gt.atest <- function(x,
                        footnote_col="footnote",
                        rowname_col=c(),
                        groupname_col=c(),
                        simplify = TRUE,
                        row_group_as_column = TRUE,
                        ...) {
  xx <- separate_about(x)
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
    select(!starts_with("_decimals_")) |> select(-any_of("_estimate_")) |>
    gt(groupname_col=groupname_col, rowname_col=rowname_col,
       row_group_as_column=row_group_as_column,
       row_group.sep=", ", ...) |>
    fmt_number(decimals = 2, columns = where(is.double) & !any_of(c("p.value", "p.adjust"))) |>
    cols_align_decimal(where(is.double) & !any_of(c("p.value", "p.adjust"))) |>
    fmt_pvalue() |>
    sub_missing(missing_text="") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_options(table.align='left')

  decimal_columns <- str_subset(names(d), "^_decimals_")
  for(col in decimal_columns) {
    n <- str_remove(col, "^_decimals_")
    if(n!="estimate_" && !n %in% names(d)) next
    dec <- d[[col]]
    n <- if(n=="estimate_") d[["_estimate_"]] else rep(n, nrow(d))
    for(k in seq_len(nrow(d))) {
      nk <- n[k]
      if(!is.na(dec[k]) && !is.na(d[[nk]][k])) {
        out <- out |> fmt_number(columns = all_of(nk), rows=k, decimals=dec[k])
      }
    }
  }

  if(!is.null(title)) {
    out <- out |> tab_header(title=title)
  }
  if(!is.null(a)) {
    out <- out |> tab_footnotes(a$footnote.text, footnote_col, a$.row)
  }

  out
}

#' @export
as_gt.data.frame <- function(x, ...) {
  gt::gt(x, ...)
}

#' @rdname as_gt
#' @export
as_gt <- function(x, ...) { UseMethod("as_gt") }
