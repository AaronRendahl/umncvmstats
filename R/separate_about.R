#' @importFrom tibble enframe
#' @importFrom forcats as_factor
separate_about <- function(x, footnotes=c("default", "text", "below")) {
  # returns a list with two parts:
  # - result: the atest without the "about" column, but with ".row" added
  # -  about: the notes, with columns ".row", "footnote.num", "footnote.text"
  # if text, no ".row" column, instead "footnotes", also about is a vector of the notes
  # if below, add them as the first column, named footnotes
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
    about <- about |> select("footnote.num", "footnote.text") |> unique() |>
      arrange(.data$footnote.num) |>
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
