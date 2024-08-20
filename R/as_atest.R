as_atest <- function(x, ...) {
  UseMethod("as_atest")
}

#' @importFrom purrr is_list
#' @export
as_atest.data.frame <- function(x,
                                about.vars=attr(x, "about.vars"),
                                estimate.vars=x[["_estimate_"]],
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
    xdf <- x[["df"]]
    dfok <- !is.na(xdf) & !is.infinite(xdf)
    xdf <- x[["df"]][dfok]
    if(length(xdf) > 0 && all(abs(round(xdf) - xdf) < 1e-6)) x[["df"]] <- as.integer(round(x[["df"]]))
  }
  if(!is.null(estimate.vars)) {
    x[["_estimate_"]] <- estimate.vars
  }
  vars1 <- c(".y", ".y_value", ".y_contrast", ".terms", ".x", ".x_value", ".x_contrast", ".g", ".g_value")
  vars2 <- c(by.vars, pri.vars, about.vars, setdiff(unique(estimate.vars), NA))
  vars3 <- c("SE", "df", "conf.low", "conf.high")
  vars4 <- inference.vars
  vars5 <- c("p.value", "p.adjust", "cld.group", "about", "_estimate_")
  varsX <- setdiff(names(x), c(vars1, vars2, vars3, vars4, vars5)) |>
    str_subset("^_decimals_", negate=TRUE)
  if(length(varsX)>0) {
    warning("Internal warning: variable type unclear for: ", paste(varsX, collapse=", "))
  }
  x <- x |> select(any_of(c(vars1, vars2, varsX, vars3, vars4, vars5)), everything())
  if(!inherits(x, "atest")) class(x) <- c("atest", class(x))
  attr(x, "about.vars") <- about.vars
  attr(x, "inference.vars") <- inference.vars
  attr(x, "pri.vars") <- pri.vars
  attr(x, "by.vars") <- by.vars
  x |> set_digits(by=c("SE", "ME"), overwrite=FALSE)
}

#' @export
as_atest.summary_emm <- function(x, model, ...) {
  about <- attr(x, "mesg")
  clNames <- attr(x, "clNames")
  attr.orig <- attributes(x)

  ## this loses attributes and class information
  x <- bind_cols(model_form(model), x)

  about <- unique(about)
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
