#' @importFrom stats p.adjust
#' @importFrom utils combn
#' @importFrom forcats fct_rev
#' @importFrom purrr map_dfr
#' @noRd
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
    result <- result |> arrange(.y) |>
      mutate(p.adjust=p.adjust(.data$p.value, method=adjust),
             p.adjust.n=n(),
             .by=any_of(".y", ".g", ".g_value"))
    method_txt <- case_when(adjust=="holm" ~ "Bonferroni-Holm",
                            adjust=="bonferroni" ~ "Bonferroni")
    adjust_txt <- sprintf("p-values adjusted for %d multiple comparisons using the %s method.", result$p.adjust.n, method_txt)
    result$p.adjust.n <- NULL
    for(idx in seq_len(nrow(result))) result$about[[idx]] <- c(result$about[[idx]], adjust_txt[idx])
  }
  as_atest(result)
}
