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
#' @param overwrite Whether or not to overwrite an existing digits specification.
#'
#' @export
set_digits <- function(x, digits=2, decimals,
                       rows=seq_len(nrow(x)),
                       columns=c("_estimate_", "SE", "conf.low", "conf.high", "predict.low", "predict.high"),
                       by=NA, by_row=TRUE, overwrite=TRUE) {
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
        else if("predict.high" %in% names(x) & "predict.low" %in% names(x)) {
          x$ME <- NA
          x$ME <- (x$predict.high - x$predict.low)/2
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
    if(isTRUE(is.na(decimals[idx]))) {
      dec <- decimals_for(x[[col]][row], digits)
    } else {
      dec <- decimals[idx]
    }
    rowest <- x[["_estimate_"]][row]
    if(!is.na(rowest) & columns[j]==rowest) {
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
    if(overwrite || is.na(x[[colname]][row])) x[[colname]][row] <- as.integer(dec)
  }
  x
}
