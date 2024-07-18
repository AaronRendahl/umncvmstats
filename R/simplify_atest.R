
#' @importFrom tidyr pivot_longer
#' @importFrom rlang :=
simplify_atest <- function(x) {
  xx <- separate_about(x)
  d <- xx$result
  a <- xx$about

  pasteif <- function(a, b, sep, FUN=\(a, b) paste(a, b, sep=sep)) {
    if_else(is.na(b), a, FUN(a, b))
  }

  nn <- c(".y", ".y_value", ".y_contrast", ".terms", ".x", ".x_value", ".x_contrast", ".g", ".g_value")
  empty <- rep(list(character()), length(nn)) |> setNames(nn) |> as_tibble()
  d <- bind_rows(d, empty)

  if(any(with(d, !is.na(.y) & !is.na(.y_contrast)))) {stop("Internal error: can't have both .y and .y_contrast")}
  if(any(with(d, !is.na(.x_value) & !is.na(.x_contrast)))) {stop("Internal error: can't have both .x_value and .x_contrast")}
  if(any(with(d, !is.na(.terms) & !is.na(.y_contrast)))) {stop("Internal error: can't have both .terms and .y_contrast")}
  if(any(with(d, !is.na(.terms) & (!is.na(.x) | !is.na(.x_value) | !is.na(.x_contrast))))) { stop("Internal error: can't have both .terms and an .x value") }
  if(any(with(d, !is.na(.terms) & (!is.na(.g) | !is.na(.g_value))))) { stop("Internal error: can't have both .terms and a .g value") }
  if(any(with(d, !is.na(.terms) & !is.na(.y_contrast)))) {stop("Internal error: can't have both .terms and .y_contrast")}

  out <- d |> mutate(.Y = case_when(!is.na(.terms) ~ NA_character_,
                                   !is.na(.y_value) ~ pasteif(.y, .y_value, " = "),
                                   !is.na(.y_contrast) ~ pasteif(.y_contrast, .y_value, FUN=\(a, b) paste(b, a, sep=": ")),
                                   TRUE ~ .y),
                     .X = case_when(!is.na(.x_value) ~ pasteif(.x, .x_value, " = "),
                                   !is.na(.x_contrast) ~ pasteif(.x_contrast, .x, FUN=\(a, b) paste(b, a, sep=": ")),
                                   TRUE ~ .x),
                     .G = pasteif(.g, .g_value, " = "),
                     .M = if_else(!is.na(.terms), pasteif(pasteif(.y, .y_value, " = "), .terms, " ~ "), NA_character_),
                     .after=".row") |>
    select(-any_of(nn))

  Nx <- out |> select(".Y", ".X", ".G", ".M") |> pivot_longer(everything()) |> summarize(n=length(unique(value[!is.na(value)])), .by="name")
  Z <- Nx |> filter(n==0) |> pull(name)
  N <- setNames(as.list(Nx$n), str_sub(Nx$name, 2, 2))

  out <- out |> select(-any_of(Z))


  if(N$M > 1) {                             # we have > 1 model, use that as grouping factor
    out <- out |> rename(.GROUP = ".M")
  } else if(N$G > 0) {                      # otherwise use grouping factor, if exists
    out <- out |> rename(.GROUP = ".G")
  }
  if(N$M == 1) out <- out |> select(-".M")  # if only one model, don't show model info

  out |> rename(any_of(c(response=".Y", variable=".X", group=".G", model=".M")))
}
