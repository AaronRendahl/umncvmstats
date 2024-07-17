#' @importFrom tibble as_tibble
#' @export
as_tibble.atest <- function(x, footnotes=c("byrow", "below", "asis"), ...) {
  footnotes <- match.arg(footnotes)
  footnotes.exist <- "about" %in% names(x)
  if(footnotes.exist && footnotes=="byrow") {
    x <- x |> mutate(about=sapply(.data$about, paste, collapse=" "))
  } else if(footnotes.exist && footnotes=="below") {
    x <- separate_about(x, footnotes="below")
  }
  x |> rm_class("atest")
}

#' @param footnote_col XX
#' @param rowname_col XX
#' @param simplify XX
#' @param row_group_as_column XX
#' @rdname as_gt
#' @export
as_gt.atest <- function(data,
                        footnote_col="footnote",
                        rowname_col=".rowname",
                        simplify = TRUE,
                        row_group_as_column = TRUE,
                        ...) {
  xx <- separate_about(data)
  d <- xx$result
  a <- xx$about
  if(isTRUE(simplify)) d <- simplify_atest(d)

  title <- attr(d, "title")
  em.groups <- attr(d, "by.vars")
  if(".group" %in% names(d)) {
    groupname_col <- ".group"
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

#' @export
print.atest <- function(x, as_gt=TRUE, ...) {
  if(isTRUE(as_gt)) {
    a <- as_gt(x)
    print(a)
    invisible(a)
  } else {
    a <- separate_about(x, footnotes="text")
    print(a$result)
    cat(a$about, sep="\n")
    invisible(x)
  }
}
