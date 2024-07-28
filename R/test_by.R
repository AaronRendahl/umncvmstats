#' @importFrom stats as.formula
#' @importFrom purrr map2
#' @noRd
test_by <- function(by_right=FALSE) {
  m <- match.call(
    definition=sys.function(sys.parent()),
    call=sys.call(sys.parent()),
    expand.dots=TRUE,
    envir=parent.frame(2L))
  FUN <- format(m[[1]]) |> str_remove("\\.formula$")
  formula <- as.formula(m[["formula"]])
  params <- as.list(m)[-1]
  params <- params[names(params)!="formula"]

  ## first split the formula, if more than one found, rerun each separately
  fs <- split_formula(formula)
  if(length(fs) > 1) {
    # cat("+ ", format(m), "\n")
    return(map(fs, function(x) do.call(FUN, c(list(x), params))) |> combine_tests_list())
  }

  ## otherwise see if there are groups, if so, make subsets and rerun
  ## if not, just return NULL and let the function run as is.
  a <- parse_formula(formula=formula)
  if(any(a$side=="group")) {
    by_name <- "group"
  } else if(isTRUE(by_right) && any(a$side=="right")) {
    by_name <- "right"
  } else {
    # cat("> ", format(m), "\n")
    return(NULL)
  }

  # cat("| ", format(m), "\n")
  data <- eval(m$data)
  f <- parse_formula(formula=formula, data=data)
  new.name <- f$about$var.names[f$about$side==by_name]
  subformula <- clean_formula(formula, by_name)
  result <- data |>
    mutate(.group=f$data[[by_name]]) |>
    nest(.by=".group") |>
    mutate(.x=map2(.data$data, .data$.group, \(.x, .g) {
      paramsi <- params
      paramsi$data <- .x
      paramsi <- c(list(subformula), paramsi)
      do.call(FUN, paramsi) |> mutate(.g_value=.g, .g=new.name)
    })) |> pull(".x") |> combine_tests_list()
  if(by_name=="right") {
    result <- result |> rename(.x_value=".g_value", .x=".g")
  }
  result
}
