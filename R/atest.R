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
as_tibble.atest <- function(x, footnotes=c("byrow", "below", "asis"), ...) {
  footnotes <- match.arg(footnotes)
  footnotes.exist <- "about" %in% names(x)
  if(footnotes.exist && footnotes=="byrow") {
    x <- x |> mutate(about=sapply(.data$about, paste, collapse=" "))
  } else if(footnotes.exist && footnotes=="below") {
    a <- x |> separate_atest()
    if(is.null(a$footnotes)) {
      x <- a$results
    } else {
      if(all(is.na(a$footnotes$footnote.num))) {
        a$footnotes$footnote.num <- NULL
      }
      n1 <- names(a$results)
      n2 <- names(a$footnotes)
      x <- bind_rows(a) |> select(all_of(c(n2, n1)))
    }
  }
  x |> rm_class("atest")
}

#' @export
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

#' @param footnote_col XX
#' @param rowname_col XX
#' @param row_group.sep XX
#' @export
#' @rdname as_gt
as_gt.atest <- function(data,
                     footnote_col="footnote",
                     rowname_col="group",
                     row_group.sep=" - ", ...) {
  x <- data
  aa <- detach_about(x)
  notes <- aa$about
  result <- aa$result
  nresponse <- nvariable <- 0
  if("response" %in% names(result)) {
    nresponse <- length(unique(result$response))
  }
  if("variable" %in% names(result)) {
    nvariable <- length(unique(result$variable))
  }
  groupname_col <- c()
  title <- NULL
  if(nresponse==1) {
    title <- result$response[1]
    result$response <- NULL
    groupname_col <- "variable"
  } else if(nresponse>1 && nvariable==1) {
    title <- sprintf("by '%s'", result$variable[1])
    result$variable <- NULL
    groupname_col <- "response"
  } else if(nresponse > 1 & nvariable > 1) {
    groupname_col = c("response", "variable")
    row_group.sep = ", by "
  } else if(nresponse > 1) {
    groupname_col <- "response"
  } else if(nvariable > 0) {
    groupname_col <- "variable"
  }
  if(!footnote_col %in% names(result)) {
    result[[footnote_col]] <- ""
  }
  result |> select(-any_of("row")) |>
    gt(groupname_col=groupname_col, rowname_col=rowname_col,
       row_group.sep=row_group.sep, ...) |>
    tab_footnotes(notes$about, footnote_col, notes$row) |>
    tab_header(title=title) |>
    fmt_numbers(n_sigfig = 2) |>
    fmt_pvalue() |>
    sub_missing(missing_text="") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_options(table.align='left')
}

separate_atest <- function(x) {
  if(! "about" %in% names(x)) {
    x <- x |> rm_class("atest")
    list(results=x, footnotes=NULL)
  } else {
    aa <- detach_about(x)
    notes <- aa$about |> summarize(footnote=paste(.data$footnote, collapse=","), .by="row")
    if(!is.na(all(notes$row))) {
      result <- aa$result |> left_join(notes, by="row") |> select(-"row")
    } else {
      result <- aa$result |> select(-"row")
    }
    # about <- aa$about |> select("footnote", "about") |> unique() |>
    #   mutate(about=if_else(is.na(.data$footnote), .data$about, paste(.data$footnote, .data$about))) |>
    #   pull("about")
    footnotes <- aa$about |> select(footnote.num="footnote", footnote.text="about") |> unique()
    list(results=result, footnotes=footnotes)
  }
}

#' @importFrom knitr knit_print
#' @importFrom knitr normal_print
#' @export
knit_print.atest <- function(x, options, inline=FALSE, ...) {
  if(isFALSE(options$as_gt)) {
    print(x, as_gt=FALSE)
  } else {
    knit_print(as_gt(x), options=options, inline=inline, ...)
  }
}

#' @export
print.atest <- function(x, as_gt=TRUE, ...) {
  if(isTRUE(as_gt)) {
    a <- as_gt(x)
    print(a)
    invisible(a)
  } else {
    a <- separate_atest(x)
    print(a$results)
    if(!is.null(a$footnotes)) {
      about <- a$footnotes |>
        mutate(about=if_else(is.na(.data$footnote.num),
                             .data$footnote.text,
                             paste(.data$footnote.num, .data$footnote.text))) |>
        pull("about")
      cat(about, sep="\n")
    }
    invisible(a)
  }
}
#' @importFrom broom tidy
#' @importFrom tibble enframe
#' @importFrom forcats as_factor
detach_about <- function(x) {
  x <- x |> rm_class("atest")
  if(!"about" %in% names(x)) {
    return(list(result=x, about=NULL))
  }
  nn <- x |> pull("about") |> unique()
  all.same <- length(nn)==1
  x <- x |>
    mutate(row=1:n(), .before=1) |>
    mutate(about=map(.data$about, enframe, name="order", value="about"))
  notes <- x |>
    select("row", "about") |>
    unnest("about") |>
    mutate(order=(.data$order-1)/(max(n(),2)-1), .before="row") |>
    mutate(order=mean(.data$order), .by="about") |>
    arrange(.data$order) |> mutate(order=as.integer(as_factor(.data$about))) |>
    mutate(footnote=.data$order, .after="order") |> select(-"order")
  if(all.same) {
    notes$row <- NA
    notes$footnote <- NA
  }
  notes <- unique(notes)
  x <- x |> select(-"about")
  list(result=x, about=notes)
}
