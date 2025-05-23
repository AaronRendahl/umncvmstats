#' @importFrom stats as.formula
#' @importFrom purrr map2
#' @noRd
test_by <- function(by_right=FALSE) {
  m <- match.call(
    definition=sys.function(sys.parent()),
    call=sys.call(sys.parent()),
    expand.dots=TRUE,
    envir=parent.frame(2L))
  FUN <- format(m[[1]]) |> str_remove("^umncvmstats:+")
  m[[1]] <- str2lang(paste0("umncvmstats:::", FUN))
  env <- parent.frame(2L)
  formula <- as.formula(m[["formula"]])

  ## first split the formula, if more than one found, rerun each separately
  fs <- split_formula(formula)
  if(length(fs) > 1) {
    out <- lapply(fs, function(x) {
      m[["formula"]] <- x
      eval(m, env)
    }) |> combine_tests_list()
    return(out)
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
  data <- eval(m$data)
  f <- parse_formula(formula=formula, data=data)
  new.name <- f$about$var.names[f$about$side==by_name]
  subformula <- clean_formula(formula, by_name)
  dsplit <- data |>
    mutate(.group=f$data[[by_name]] |> forcats::as_factor() |> forcats::fct_na_value_to_level("(Missing)")) |>
    nest(.by=".group") |> arrange(.data$.group)
  result <- mapply(\(.x, .g) {
    m[["formula"]] <- subformula
    m[["data"]] <- .x
    eval(m, env) |> mutate(.g_value=.g, .g=new.name)
  }, dsplit$data, dsplit$.group, SIMPLIFY=FALSE) |> combine_tests_list()
  if(by_name=="right") {
    result <- result |> rename(.x_value=".g_value", .x=".g")
  }
  result
}
