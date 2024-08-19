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
    }
    by <- intersect(by, names(x))
    for(b in by) {
      d_by <- decimals_for(x[[b]][rows], digits)
      decimals <- pmax(decimals, d_by, na.rm=TRUE)
    }
    x$ME <- NULL
    if(!by_row) decimals <- max(decimals, na.rm=TRUE)
  }
  numeric.vals <- names(x)[sapply(x, is.double)]
  if(!any(columns %in% numeric.vals)) return(x)
  columns <- intersect(columns, numeric.vals)
  if(length(decimals)==1) decimals <- rep(decimals, length(rows))
  if(length(decimals)!=length(rows)) stop("length of decimals must be either 1 or length of rows")
  colnames <- paste0("_decimals_", columns)
  for(col in setdiff(colnames, names(x))) x[[col]] <- NA
  for(idx in seq_along(rows)) for(j in seq_along(columns)) {
    if(isTRUE(is.na(by))) {
      dec <- decimals_for(x[[columns[idx]]][rows[idx]], digits)
    } else {
      dec <- decimals[idx]
    }
    x[[colnames[j]]][rows[idx]] <- as.integer(dec)
  }
  x
}
