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

#' @export
as_atest.data.frame <- function(x, ...) {
  x <- as_tibble(x)
  varlist <- c("by", "by.value", "response", "response.value", "variable", "value")
  x <- x |> select(any_of(varlist), everything())
  if(!inherits(x, "atest")) class(x) <- c("atest", class(x))
  x
}

#' @export
tidy.atest <- function(x, ...) {
  x |> mutate(about=sapply(.data$about, paste, collapse=" ")) |>
    rm_class("atest")
}

#' Create a gt table object
#' @param data XX
#' @param ... XX
#'
#' @export
gt <- function(data, ...) { UseMethod("gt") }

#' @export
#' @rdname gt
gt.default <- function(data, ...) { gt::gt(data, ...)}

tab_footnotes <- function(data, notes, columns=NA, rows=NA) {
  aa <- tibble(note=notes, columns=columns, rows=rows) |>
    left_join(data$`_boxhead` |> select(columns="var", "type"), .by="columns")
  if(any(aa$type=="hidden")) {
    hidden_cols <- aa |> filter(.data$type=="hidden") |>
      pull("columns") |>
      (\(x) sprintf("'%s'", x))() |>
      (\(x) paste(x, collapse=", "))()
    warning(sprintf("footnote column %s is hidden"), hidden_cols)
  }
  for(idx in seq_len(nrow(aa))) {
    r <- aa$rows[[idx]]
    r <- r[!is.na(r)]
    if(length(r)==0) {
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
#' @rdname gt
gt.atest <- function(data,
                     footnote_col="group",
                     rowname_col="group",
                     row_group.sep=" - ", ...) {
  x <- data
  aa <- detach_about(x)
  notes <- aa$about |> summarize(rows=list(.data$row), .by="about")
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

  r <- unlist(notes$rows)
  r <- r[!is.na(r)]
  anyr <- length(r) > 0
  if(anyr && (!footnote_col %in% names(result))) {
    result <- result |> mutate(footnote="")
    footnote_col <- "footnote"
  }
  result |> select(-row) |>
    gt(groupname_col=groupname_col, rowname_col=rowname_col,
       row_group.sep=row_group.sep, ...) |>
    tab_footnotes(notes$about, footnote_col, notes$rows) |>
    tab_header(title=title) |>
    fmt_number(where(is.double), n_sigfig = 2) |>
    fmt_pvalue() |>
    sub_missing(missing_text="") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5)
}

#' @export
print.atest <- function(x, ...) {
  if(! "about" %in% names(x)) {
    x <- x |> rm_class("atest")
    print(x)
    invisible(list(result=x, about=NULL))
  } else {
    aa <- detach_about(x)
    notes <- aa$about |> summarize(footnote=paste(.data$footnote, collapse=","), .by="row")
    if(!is.na(all(notes$row))) {
      result <- aa$result |> left_join(notes, by="row") |> select(-"row")
    } else {
      result <- aa$result |> select(-"row")
    }
    about <- aa$about |> select("order", "footnote", "about") |> unique() |>
      arrange(.data$order) |>
      mutate(about=if_else(is.na(.data$footnote), .data$about, paste(.data$footnote, .data$about))) |>
      pull("about")
    print(result)
    cat(about, sep="\n")
    invisible(list(result=result, about=about))
  }
}

#' @importFrom broom tidy
#' @importFrom tibble enframe
#' @importFrom forcats as_factor
detach_about <- function(x) {
  nn <- tidy(x) |> pull("about") |> unique()
  all.same <- length(nn)==1
  x <- x |>
    rm_class("atest") |>
    mutate(row=1:n(), .before=1) |>
    mutate(about=map(.data$about, enframe, name="order", value="about"))
  notes <- x |>
    select("row", "about") |>
    unnest("about") |>
    mutate(order=(.data$order-1)/(max(n(),2)-1), .before="row") |>
    mutate(order=mean(.data$order), .by="about") |>
    arrange(.data$order) |> mutate(order=as.integer(as_factor(.data$about))) |>
    mutate(footnote=.data$order, .after="order")
  if(all.same) {
    notes$row <- NA
    notes$footnote <- NA
  }
  x <- x |> select(-"about")
  list(result=x, about=notes)
}
