separate_atest <- function(x) {
  if(! "about" %in% names(x)) {
    x <- x |> rm_class("atest")
    list(results=x, footnotes=NULL)
  } else {
    aa <- detach_about(x)
    notes <- aa$about |> summarize(footnote=paste(.data$footnote, collapse=","), .by="row")
    if(!is.na(all(notes$row))) {
      result <- aa$result |> left_join(notes, by="row") |> select(-"row")
    } else {
      result <- aa$result |> select(-"row")
    }
    footnotes <- aa$about |> select(footnote.num="footnote", footnote.text="about") |> unique()
    list(results=result, footnotes=footnotes)
  }
}

#' @importFrom broom tidy
#' @importFrom tibble enframe
#' @importFrom forcats as_factor
detach_about <- function(x) {
  x <- x |> rm_class("atest")
  if(!"about" %in% names(x)) {
    return(list(result=x, about=NULL))
  }
  nn <- x |> pull("about") |> unique()
  all.same <- length(nn)==1
  x <- x |>
    mutate(row=1:n(), .before=1) |>
    mutate(about=map(.data$about, enframe, name="order", value="about"))
  notes <- x |>
    select("row", "about") |>
    unnest("about") |>
    mutate(order=(.data$order-1)/(max(n(),2)-1), .before="row") |>
    mutate(order=mean(.data$order), .by="about") |>
    arrange(.data$order) |> mutate(order=as.integer(as_factor(.data$about))) |>
    mutate(footnote=.data$order, .after="order") |> select(-"order")
  if(all.same) {
    notes$row <- NA
    notes$footnote <- NA
  }
  notes <- unique(notes)
  x <- x |> select(-"about")
  list(result=x, about=notes)
}
