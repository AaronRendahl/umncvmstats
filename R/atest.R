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
  x <- x |> rename(any_of(c(SE="std.error")))

  attr.keep <- c("estName", "pri.vars", "by.vars")
  attr.keep <- attr.keep[attr.keep %in% names(attr.orig)]
  for(a in attr.keep) attr(x, a) <- attr.orig[[a]]

  as_atest(x, ...)
}

#' @importFrom purrr is_list
#' @export
as_atest.data.frame <- function(x,
                                estimate.vars=attr(x, "estimate.vars"),
                                inference.vars=attr(x, "inference.vars"),
                                pri.vars=attr(x, "pri.vars"),
                                by.vars=attr(x, "by.vars"),
                                ...) {
  if("about" %in% names(x) && !is_list(x[["about"]])) {
    warning("Internal warning: converting messages to a list.")
    x[["about"]] <- list(x[["about"]])
  }
  if(!inherits(x, "tbl")) { x <- as_tibble(x) }
  if("df" %in% names(x)) {
    xdf <- x[["df"]][!is.na(x[["df"]])]
    if(length(xdf) > 0 && all(abs(round(xdf) - xdf) < 1e-6)) x[["df"]] <- as.integer(round(x[["df"]]))
  }
  vars1 <- c(".y", ".y_value", ".y_contrast", ".terms", ".x", ".x_value", ".x_contrast", ".g", ".g_value")
  vars2 <- c(by.vars, pri.vars, estimate.vars)
  vars3 <- c("conf.low", "conf.high")
  vars4 <- inference.vars
  vars5 <- c("p.value", "p.adjust", "cld.group", "about")
  varsX <- setdiff(names(x), c(vars1, vars2, vars3, vars4, vars5))
  if(length(varsX)>0) {
    message("Internal warning: variable type unclear for: ", paste(varsX, collapse=", "))
  }
  x <- x |> select(any_of(c(vars1, vars2, varsX, vars3, vars4, vars5)), everything())
  if(!inherits(x, "atest")) class(x) <- c("atest", class(x))
  attr(x, "estimate.vars") <- estimate.vars
  attr(x, "inference.vars") <- inference.vars
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

