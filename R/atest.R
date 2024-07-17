#' Combine multiple tests together
#'
#' @param ... XX
#'
#' @export
combine_tests <- function(...) {
  bind_rows(...) |> as_atest()
}

rm_class <- function(x, class) { class(x) <- setdiff(class(x), class); x }

as_atest <- function(x, ...) {
  UseMethod("as_atest")
}

#' @export
as_atest.summary_emm <- function(x, model, ...) {
  about <- attr(x, "mesg")
  clNames <- attr(x, "clNames")
  attr.orig <- attributes(x)

  ## this loses attributes and class information
  x <- bind_cols(model_form(model), x)

  skip <- "NOTE: If two or more means share the same grouping symbol,\n      then we cannot show them to be different.\n      But we also did not show them to be the same."
  about <- unique(about[about!=skip])
  x$about <- rep(list(about), nrow(x))

  if(length(clNames)==2) {
    names(x)[match(clNames, names(x))] <- c("conf.low", "conf.high")
  }

  attr.keep <- c("estName", "pri.vars", "by.vars")
  attr.keep <- attr.keep[attr.keep %in% names(attr.orig)]
  for(a in attr.keep) attr(x, a) <- attr.orig[[a]]

  as_atest(x)
}

#' @importFrom purrr is_list
#' @export
as_atest.data.frame <- function(x, ...) {
  if(!inherits(x, "tbl")) { x <- as_tibble(x) }
  if("df" %in% names(x)) {
    xdf <- x[["df"]][!is.na(x[["df"]])]
    if(length(xdf) > 0 && all(abs(round(xdf) - xdf) < 1e-6)) x[["df"]] <- as.integer(round(x[["df"]]))
  }
  x <- x |> rename(any_of(c(SE="std.error")))
  vars1 <- c("group", "group.value", "response", "response.value", "variable", "value")
  vars3 <- c("p.value", "p.adjust", "about")
  vars2 <- setdiff(names(x), c(vars1, vars3))
  x <- x |> select(any_of(c(vars1, vars2, vars3)), everything())
  if(!inherits(x, "atest")) class(x) <- c("atest", class(x))
  x
}

#' @export
#' @importFrom broom tidy
tidy.atest <- function(x, ...) {
  as_tibble.atest(x, ...)
}

#' @export
as.data.frame.atest <- function(x, ...) {
  as_tibble.atest(x, ...) |> as.data.frame()
}

#' Create a gt table object
#' @param data XX
#' @param ... XX
#'
#' @export
as_gt <- function(data, ...) { UseMethod("as_gt") }

#' @export
#' @rdname as_gt
as_gt.default <- function(data, ...) {
  if(inherits(data, "gt_tbl")) {
    data
  } else {
    gt::gt(data, ...)
  }
}

tab_footnotes <- function(data, notes, columns=NA, rows=NA) {
  if(missing(notes) || is.null(notes)) return(data)
  aa <- tibble(note=notes, columns=columns, rows=rows) |>
    left_join(data$`_boxhead` |> select(columns="var", "type"), by="columns")
  hidden_cols <- aa |> filter(!is.na(.data$rows) & (is.na(.data$type) | .data$type=="hidden"))
  if(nrow(hidden_cols)>0) {
    hidden_txt <- hidden_cols |>
      pull("columns") |>
      (\(x) sprintf("'%s'", x))() |>
      (\(x) paste(x, collapse=", "))()
    warning(sprintf("footnote column %s is missing or hidden", hidden_txt))
  }
  for(idx in seq_len(nrow(aa))) {
    r <- aa$rows[idx]
    if(is.na(r)) {
      data <- data |> tab_footnote(aa$note[idx])
    } else {
      if(aa$type[idx]=="stub") {
        data <- data |> tab_footnote(aa$note[idx], cells_stub(r))
      } else {
        data <- data |> tab_footnote(aa$note[idx], cells_body(aa$columns[idx], r))
      }
    }
  }
  data
}

#' @importFrom knitr knit_print
#' @export
knit_print.atest <- function(x, options, inline=FALSE, ...) {
  if(isFALSE(options$as_gt)) {
    print(x, as_gt=FALSE)
  } else {
    knit_print(as_gt(x), options=options, inline=inline, ...)
  }
}

