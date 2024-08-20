#' @importFrom tibble as_tibble
#' @export
as_tibble.atest <- function(x, footnotes=c("byrow", "below", "asis"), ...) {
  footnotes <- match.arg(footnotes)
  footnotes.exist <- "about" %in% names(x)
  if(footnotes.exist && footnotes=="byrow") {
    x <- x |> mutate(about=sapply(.data$about, paste, collapse=" "))
  } else if(footnotes.exist && footnotes=="below") {
    x <- separate_about(x, footnotes="below")
  }
  x |> rm_class("atest") |> select(-starts_with("_decimals"), -any_of("_estimate_"))
}

#' @export
print.atest <- function(x, as_gt=TRUE, ...) {
  if(isTRUE(as_gt)) {
    a <- as_gt(x)
    print(a)
    invisible(a)
  } else {
    a <- separate_about(x, footnotes="text")
    out <- a$result |> select(-starts_with("_decimals"), -any_of("_estimate_"))
    print(out)
    cat(a$about, sep="\n")
    invisible(x)
  }
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

#' @importFrom knitr knit_print
#' @export
knit_print.atest <- function(x, options, inline=FALSE, ...) {
  if(isFALSE(options$as_gt)) {
    print(out, as_gt=FALSE)
  } else {
    knit_print(as_gt(x), options=options, inline=inline, ...)
  }
}
