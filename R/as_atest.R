as_atest <- function(x, ...) {
  UseMethod("as_atest")
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
    xdf <- x[["df"]]
    dfok <- !is.na(xdf) & !is.infinite(xdf)
    xdf <- x[["df"]][dfok]
    if(length(xdf) > 0 && all(abs(round(xdf) - xdf) < 1e-6)) x[["df"]] <- as.integer(round(x[["df"]]))
  }
  vars1 <- c(".y", ".y_value", ".y_contrast", ".terms", ".x", ".x_value", ".x_contrast", ".g", ".g_value")
  vars2 <- c(by.vars, pri.vars, estimate.vars)
  vars3 <- c("SE", "df", "conf.low", "conf.high")
  vars4 <- inference.vars
  vars5 <- c("p.value", "p.adjust", "cld.group", "about")
  varsX <- setdiff(names(x), c(vars1, vars2, vars3, vars4, vars5)) |>
    str_subset("^_decimals_", negate=TRUE)
  if(length(varsX)>0) {
    warning("Internal warning: variable type unclear for: ", paste(varsX, collapse=", "))
  }
  x <- x |> select(any_of(c(vars1, vars2, varsX, vars3, vars4, vars5)), everything())
  if(!inherits(x, "atest")) class(x) <- c("atest", class(x))
  attr(x, "estimate.vars") <- estimate.vars
  attr(x, "inference.vars") <- inference.vars
  x
}

decimals_for <- function(x, digits=2) {
  n <- floor(log10(abs(x))) + 1 - digits
  nc <- nchar(as.character(round(x/10^n)))
  n <- if_else(nc > digits, n + 1, n)
  pmax(-n, 0)
}


#' Set desired digits for values in an atest
#'
#' @param x The atest object
#' @param digits The desired number of significant digits.
#' @param decimals Alternatively, the number of decimal places to show.
#' @param rows The rows to apply this to
#' @param columns The columns to apply this to; by default, the columns
#' corresponding to the estimate, the SE, and the confidence intervals.
#' @param by The column that determines the significant digits, by default the SE.
#' @param by_row Whether or not to compute the significant digits by row (the default),
#' or to use the number of digits such that all rows have at least this many.
#'
#' @export
set_digits <- function(x, digits=2, decimals,
                       rows=seq_len(nrow(x)),
                       columns=c(attr(x, "estimate.vars"), "SE", "conf.low", "conf.high"),
                       by="SE", by_row=TRUE) {
  if(missing(decimals)) {
    decimals <- decimals_for(x[[by]][rows], digits)
    if(!by_row) decimals <- max(decimals)
  }
  numeric.vals <- names(x)[sapply(x, is.double)]
  if(!any(columns %in% numeric.vals)) return(x)
  columns <- intersect(columns, numeric.vals)
  if(length(decimals)==1) decimals <- rep(decimals, length(rows))
  if(length(decimals)!=length(rows)) stop("length of decimals must be either 1 or length of rows")
  colnames <- paste0("_decimals_", columns)
  for(col in setdiff(colnames, names(x))) x[[col]] <- NA
  for(idx in seq_along(rows)) for(col in colnames) {
    x[[col]][rows[idx]] <- as.integer(decimals[idx])
  }
  x
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
