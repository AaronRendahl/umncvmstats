#' Combine multiple tests together
#'
#' @param ... the various tests to be combined.
#'
#' @export
combine_tests <- function(...) {
  x <- list(...)
  estimate.vars <- lapply(x, \(xi) attr(xi, "estimate.vars")) |> unlist() |> unique()
  inference.vars <- lapply(x, \(xi) attr(xi, "inference.vars")) |> unlist() |> unique()
  pri.vars <- lapply(x, \(xi) attr(xi, "pri.vars")) |> unlist() |> unique()
  by.vars <- lapply(x, \(xi) attr(xi, "by.vars")) |> unlist() |> unique()
  bind_rows(x) |> as_atest(estimate.vars=estimate.vars, inference.vars=inference.vars,
                           pri.vars=pri.vars, by.vars=by.vars)
}

combine_tests_list <- function(x) {
  estimate.vars <- lapply(x, \(xi) attr(xi, "estimate.vars")) |> unlist() |> unique()
  inference.vars <- lapply(x, \(xi) attr(xi, "inference.vars")) |> unlist() |> unique()
  pri.vars <- lapply(x, \(xi) attr(xi, "pri.vars")) |> unlist() |> unique()
  by.vars <- lapply(x, \(xi) attr(xi, "by.vars")) |> unlist() |> unique()
  bind_rows(x) |> as_atest(estimate.vars=estimate.vars, inference.vars=inference.vars,
                           pri.vars=pri.vars, by.vars=by.vars)
}
