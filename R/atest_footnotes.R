#' @importFrom tibble enframe
#' @importFrom forcats as_factor
# returns a list with two parts:
# - result: the atest without the "about" column, but with ".row" added
# -  about: the notes, with columns ".row", "footnote.num", "footnote.text"
separate_about <- function(x) {
  x <- x |> rm_class("atest")
  if(!"about" %in% names(x)) {
    return(list(result=x, about=NULL))
  }
  x.row <- x |> mutate(.row=1:n(), .before=1)
  result <- x.row |> select(-"about")
  all.same <- length(unique(x.row[["about"]]))==1
  notes <- x.row |> select(".row", "about") |>
    mutate(about=map(.data$about, enframe, name = NULL, value = "footnote.text")) |>
    unnest("about") |>
    mutate(footnote.num=as.integer(as_factor(.data$footnote.text)), .after=".row")
  if(all.same) {
    notes <- notes |> mutate(.row=NA_integer_, footnote.num=NA_integer_) |> unique()
  }
  list(result=result, about=notes)
}

#' @export
#' @importFrom tibble as_tibble
as_tibble.atest <- function(x, footnotes=c("byrow", "below", "asis"), ...) {
  footnotes <- match.arg(footnotes)
  footnotes.exist <- "about" %in% names(x)
  if(footnotes.exist && footnotes=="byrow") {
    x <- x |> mutate(about=sapply(.data$about, paste, collapse=" "))
  } else if(footnotes.exist && footnotes=="below") {
    a <- separate_about(x)
    if(is.null(a$about)) {
      x <- a$results
    } else {
      if(all(is.na(a$about$footnote.num))) {
        a$about$footnote.num <- NULL
        a$about$.row <- NULL
      }
      n1 <- names(a$results)
      n2 <- names(a$about)
      x <- bind_rows(a) |> select(all_of(c(n2, n1)))
    }
  }
  x |> rm_class("atest")
}

#' @param footnote_col XX
#' @param rowname_col XX
#' @param row_group.sep XX
#' @export
#' @rdname as_gt
as_gt.atest <- function(data,
                        footnote_col="footnote",
                        rowname_col="group",
                        row_group.sep=" - ", ...) {
  x <- data
  aa <- separate_about(x)
  notes <- aa$about
  result <- aa$result
  nresponse <- nvariable <- 0
  if("response" %in% names(result)) {
    nresponse <- length(unique(result$response))
  }
  if("variable" %in% names(result)) {
    nvariable <- length(unique(result$variable))
  }
  groupname_col <- c()
  title <- NULL
  if(nresponse==1) {
    title <- result$response[1]
    result$response <- NULL
    groupname_col <- "variable"
  } else if(nresponse>1 && nvariable==1) {
    title <- sprintf("by '%s'", result$variable[1])
    result$variable <- NULL
    groupname_col <- "response"
  } else if(nresponse > 1 & nvariable > 1) {
    groupname_col = c("response", "variable")
    row_group.sep = ", by "
  } else if(nresponse > 1) {
    groupname_col <- "response"
  } else if(nvariable > 0) {
    groupname_col <- "variable"
  }
  if(!footnote_col %in% names(result)) {
    result[[footnote_col]] <- ""
  }
  result |> select(-any_of("row")) |>
    gt(groupname_col=groupname_col, rowname_col=rowname_col,
       row_group.sep=row_group.sep, ...) |>
    tab_footnotes(notes$about, footnote_col, notes$row) |>
    tab_header(title=title) |>
    fmt_numbers(n_sigfig = 2) |>
    fmt_pvalue() |>
    sub_missing(missing_text="") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_options(table.align='left')
}

#' @export
print.atest <- function(x, as_gt=TRUE, ...) {
  if(isTRUE(as_gt)) {
    a <- as_gt(x)
    print(a)
    invisible(a)
  } else {
    a <- separate_atest(x)
    print(a$results)
    if(!is.null(a$footnotes)) {
      about <- a$footnotes |>
        mutate(about=if_else(is.na(.data$footnote.num),
                             .data$footnote.text,
                             paste(.data$footnote.num, .data$footnote.text))) |>
        pull("about")
      cat(about, sep="\n")
    }
    invisible(a)
  }
}
