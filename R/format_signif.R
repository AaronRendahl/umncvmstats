places_for <- function(x, digits=2) {
  x <- abs(x)
  n <- floor(log10(x)) + 1 - digits
  nc <- nchar(formatC(x/10^n, digits=0, format="f"))
  out <- if_else(nc > digits, n + 1, n)
  if_else(x==0, -digits, out)
}

decimals_for <- function(x, digits=2) {
  n <- places_for(x, digits=digits)
  if_else(x==0, 0, pmax(-n, 0))
}

format_signif1 <- function(x, digits=3, max_small=6, max_big=6, keep_big=digits) {
  if(is.na(x)) return(NA_character_)
  max_small <- max(max_small, digits)
  keep_big <- min(keep_big, max_big)
  n <- places_for(x, digits=digits)
  if(n < -max_small) {
    formatC(x, digits-1, format="e")
  } else if(n <= 0) {
    formatC(x, abs(n), format="f")
  } else {
    n2 <- places_for(x, digits=max_big)
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
