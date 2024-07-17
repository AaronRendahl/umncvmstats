#' @importFrom tibble enframe
#' @importFrom forcats as_factor
# returns a list with two parts:
# - result: the atest without the "about" column, but with ".row" added
# -  about: the notes, with columns ".row", "footnote.num", "footnote.text"
# if text, no ".row" column, instead "footnotes", also about is a vector of the notes
# if below, add them as the first column, named footnotes
separate_about <- function(x, footnotes=c("default", "text", "below")) {
  footnotes <- match.arg(footnotes)
  x <- x |> rm_class("atest")
  if(!"about" %in% names(x)) {
    return(list(result=x, about=NULL))
  }
  x.row <- x |> mutate(.row=1:n(), .before=1)
  result <- x.row |> select(-"about")
  about <- x.row |> select(".row", "about") |>
    mutate(about=map(.data$about, enframe, name = NULL, value = "footnote.text")) |>
    unnest("about") |>
    mutate(footnote.num=as.integer(as_factor(.data$footnote.text)), .after=".row")
  if(length(unique(x.row[["about"]]))==1) {
    about <- about |> mutate(.row=NA_integer_, footnote.num=NA_integer_) |> unique()
  }
  ## now process if needed
  if(footnotes %in% c("below", "text")) {
    nn <- about |> summarize(footnotes=paste(.data$footnote.num, collapse=","), .by=.row)
    rr <- result |> left_join(nn, by=".row")
    about <- about |> select(footnote.num, footnote.text) |> unique() |>
      arrange(footnote.num) |>
      mutate(about=if_else(is.na(.data$footnote.num),
                           .data$footnote.text,
                           paste(.data$footnote.num, .data$footnote.text))) |>
      pull("about")
    result <- result |> select(-".row")
  }
  out <- list(result=result, about=about)
  if(footnotes=="below") {
    out <- bind_rows(result, tibble(footnotes=about)) |> select("footnotes", everything())
  }
  out
}

#' @export
#' @importFrom tibble as_tibble
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
    a <- separate_about(x, footnotes="text")
    print(a$result)
    cat(a$about, sep="\n")
    invisible(x)
  }
}
