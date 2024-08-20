
places_for <- function(x, digits=2) {
  n <- floor(log10(abs(x))) + 1 - digits
  nc <- nchar(formatC(x/10^n, digits=0, format="f"))
  if_else(nc > digits, n + 1, n)
}

decimals_for <- function(x, digits=2) {
  n <- places_for(x, digits=digits)
  if_else(x==0, 0, pmax(-n, 0))
}

format_signif1 <- function(x, digits=3, max_small=6, max_big=6, keep_big=digits) {
  max_small <- max(max_small, digits)
  keep_big <- min(keep_big, max_big)
  n <- tmpfor(x, digits=digits)
  if(n < -max_small) {
    formatC(x, digits-1, format="e")
  } else if(n <= 0) {
    formatC(x, abs(n), format="f")
  } else {
    n2 <- tmpfor(x, digits=max_big)
    if(n2 > 0) {
      formatC(x, digits=keep_big-1, format="e")
    } else {
      formatC(x, 0, format="f")
    }
  }
}

#' Format to significant digits
#'
#' @param x the numbers to format
#' @param digits XX
#' @param max_small XX
#' @param max_big XX
#' @param keep_big XX
#'
#' @export
format_signif <- function (x, digits = 3, max_small = 6, max_big = 6, keep_big = digits) {
  ## from result of
  ## Vectorize(foo, c("x", "digits", "max_small", "max_big", "keep_big"))
  vectorize.args <- c("x", "digits", "max_small", "max_big", "keep_big")
  args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
  names <- names(args) %||% character(length(args))
  dovec <- names %in% vectorize.args
  do.call("mapply", c(FUN = format_signif1, args[dovec], MoreArgs = list(args[!dovec]),
                      SIMPLIFY = TRUE, USE.NAMES = TRUE))
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
                       columns=c("_estimate_", "SE", "conf.low", "conf.high"),
                       by=c("SE", "ME"), by_row=TRUE) {
  if(missing(decimals)) {
    if(!is.numeric(digits) | any(digits<1)) stop("digits must be numeric and >= 1")
    if(isTRUE(is.na(by))) {
      decimals <- NA
    } else {
      if("ME" %in% by & !"ME" %in% names(x)) {
        if("conf.high" %in% names(x) & "conf.low" %in% names(x)) {
          x$ME <- NA
          x$ME <- (x$conf.high - x$conf.low)/2
        }
      }
      decimals <- rep(NA_integer_, length(rows))
      by <- intersect(by, names(x))
      for(b in by) {
        d_by <- decimals_for(x[[b]][rows], digits)
        decimals <- pmax(decimals, d_by, na.rm=TRUE)
      }
      x$ME <- NULL
      if(!by_row) decimals <- max(decimals, na.rm=TRUE)
    }
  }
  if("_estimate_" %in% columns) {
    columns <- c(columns, x[["_estimate_"]][rows]) |>
      setdiff(c("_estimate_", NA))
  }
  columns <- intersect(columns, names(x)[sapply(x, is.double)])
  if(length(columns)==0) return(x)
  if(length(decimals)==1) decimals <- rep(decimals, length(rows))
  if(length(decimals)!=length(rows)) stop("length of decimals must be either 1 or length of rows")
  for(idx in seq_along(rows)) for(j in seq_along(columns)) {
    col <- columns[j]
    row <- rows[idx]
    if(isTRUE(is.na(by))) {
      dec <- decimals_for(x[[col]][row], digits)
    } else {
      dec <- decimals[idx]
    }
    if(columns[j]==x[["_estimate_"]][row]) {
      colname <- "_decimals_estimate_"
    } else if(col %in% x[["_estimate_"]][-row]) {
      if(!is.na(x[[col]][row])) {
        warning(sprintf("Internal warning: missing value expected for %s, row %d", col, row))
      }
      next
    } else {
      colname <- paste0("_decimals_", col)
    }
    if(!colname %in% names(x)) x[[colname]] <- NA
    x[[colname]][row] <- as.integer(dec)
  }
  x
}
