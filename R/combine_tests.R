#' Combine multiple tests together
#'
#' @param ... the various tests to be combined.
#'
#' @export
#' @rdname combine_tests
combine_tests <- function(...) {
  x <- list(...)
  about.vars <- lapply(x, \(xi) attr(xi, "about.vars")) |> unlist() |> unique()
  inference.vars <- lapply(x, \(xi) attr(xi, "inference.vars")) |> unlist() |> unique()
  pri.vars <- lapply(x, \(xi) attr(xi, "pri.vars")) |> unlist() |> unique()
  by.vars <- lapply(x, \(xi) attr(xi, "by.vars")) |> unlist() |> unique()
  bind_rows(x) |> as_atest(about.vars=about.vars, inference.vars=inference.vars,
                           pri.vars=pri.vars, by.vars=by.vars)
}

#' @rdname combine_tests
#' @param x a list of tests to be combined.
combine_tests_list <- function(x) {
  do.call(combine_tests, x)
}
