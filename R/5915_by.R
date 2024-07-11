do_subformulas <- function(by_right=FALSE) {
  m <- match.call(
    definition=sys.function(sys.parent()),
    call=sys.call(sys.parent()),
    expand.dots=TRUE,
    envir=parent.frame(2L))
  m[["FUN"]] <- m[[1]]
  m[["by_right"]] <- by_right
  m[[1L]] <- quote(test_by)
  eval(m, parent.frame(2L))
}

#' Do multiple tests by splitting a formula
#' @param formula XX
#' @param FUN XX
#' @param ... XX
#' @param by_right XX
#'
#' @export
test_by <- function(formula, FUN, ..., by_right=FALSE) {
  fs <- split_formula(formula)
  if(length(fs) > 1) {
    return(map(fs, function(x) FUN(x, ...)) |> bind_rows())
  }
  a <- parse_formula(formula=formula)
  has_group <- any(a$side=="group")
  has_right <- any(a$side=="right")
  if(has_group) {
    test_by_group(formula, FUN, ...)
  } else if(isTRUE(by_right) && has_right) {
    test_by_right(formula, FUN, ...)
  } else {
    return(NULL)
  }
}

test_by_right <- function(formula, data, FUN, ...) {
  f <- parse_formula(formula=formula, data=data)
  right.name <- f$about$var.names[f$about$side=="right"]
  formula.l <- remove_right(formula)
  result <- data |>
    mutate(.groups=f$data$right) |>
    nest(.by=".groups") |>
    arrange(.data$.groups) |>
    mutate(.x=map(data, \(.x) {
      FUN(formula.l, data=.x, ...)
    })) |> select(-"data") |>
    unnest(".x") |>
    rename(value=".groups") |>
    mutate(variable=right.name, .before="value")
  as_atest(result)
}

test_by_group <- function(formula, data, FUN, ...) {
  f <- parse_formula(formula=formula, data=data)
  group.name <- f$about$var.names[f$about$side=="group"]
  formula.lr <- remove_group(formula)
  result <- data |>
    mutate(.groups=f$data$group) |>
    nest(.by=".groups") |>
    arrange(.data$.groups) |>
    mutate(.x=map(data, \(.x) {
      FUN(formula.lr, data=.x, ...)
    })) |> select(-data) |>
    unnest(".x") |>
    rename(group.value=".groups") |>
    mutate(group=group.name, .before="group.value")
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
  result <- map_dfr(seq_len(nrow(todo)), \(idx) FUN(formula, data=get_subset(idx), conf.adjust=conf.adjust, ...))
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
  pairwise(formula, data, two_t_test, ...)
}

#' @param ... XX
#' @rdname two_proportion_test
#' @export
pairwise_proportion_test <- function(formula, data, ...) {
  pairwise(formula, data, two_proportion_test, ...)
}

