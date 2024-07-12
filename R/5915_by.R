#' @importFrom stats as.formula
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
    return(map(fs, function(x) do.call(FUN, c(list(x), params))) |> bind_rows())
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
    mutate(.groups=f$data[[by_name]]) |>
    nest(.by=".groups") |>
    arrange(.data$.groups) |>
    mutate(.x=map(data, \(.x) {
      paramsi <- params
      paramsi$data <- .x
      paramsi <- c(list(subformula), paramsi)
      do.call(FUN, paramsi)
    })) |> select(-"data") |>
    unnest(".x")
  if(by_name=="right") {
    result <- result |> rename(value=".groups") |> mutate(variable=new.name)
  } else {
    result <- result |> rename(group.value=".groups") |> mutate(group=new.name)
  }
  as_atest(result)
}


#' @importFrom stats p.adjust
#' @importFrom utils combn
#' @importFrom forcats fct_rev
pairwise <- function(formula, data, FUN, conf.level=0.95, ..., adjust=c("bonferroni", "holm", "none"), reverse=FALSE) {
  adjust <- match.arg(adjust)
  f <- parse_formula(formula=formula, data=data)
  x.name <- f$about$var.names[f$about$side=="right"]
  if(!x.name %in% names(data)) {stop(x.name, " must be a variable in the data.")}
  if(!is.factor(data[[x.name]])) {stop(x.name, " must be a factor.")}
  if(isTRUE(reverse)) data[[x.name]] <- fct_rev(data[[x.name]])
  levs <- levels(data[[x.name]])
  todo <- t(combn(length(levs), 2))
  todo[] <- levs[todo]
  colnames(todo) <- paste0("X", 1:2)
  get_subset <- function(idx) { droplevels(data[data[[x.name]] %in% todo[idx,],]) }
  conf.adjust <- if(adjust=="none") 1 else nrow(todo)
  result <- map_dfr(seq_len(nrow(todo)), \(idx) {
    do.call(FUN, c(list(formula, data=get_subset(idx), conf.adjust=conf.adjust), list(...)))
  })
  if(nrow(todo) > 1 && adjust!="none") {
    result <- result |> mutate(p.adjust=p.adjust(.data$p.value, method=adjust))
    method_txt <- case_when(adjust=="holm" ~ "Bonferroni-Holm",
                            adjust=="bonferroni" ~ "Bonferroni")
    adjust_txt <- sprintf("p-values adjusted for %d multiple comparisons using the %s method.", nrow(todo), method_txt)
    result$about <- lapply(result$about, \(x) c(x, adjust_txt))
  }
  as_atest(result)
}


#' @param ... XX
#' @rdname two_t_test
#' @export
pairwise_t_test <- function(formula, data, ...) {
  pairwise(formula, data, "two_t_test", ...)
}

#' @param ... XX
#' @rdname two_proportion_test
#' @export
pairwise_proportion_test <- function(formula, data, ...) {
  pairwise(formula, data, "two_proportion_test", ...)
}

