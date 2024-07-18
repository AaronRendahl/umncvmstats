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
  x |> rm_class("atest")
}

#' @export
print.atest <- function(x, as_gt=TRUE, ...) {
  if(isTRUE(as_gt)) {
    a <- as_gt(x)
    print(a)
    invisible(a)
  } else {
    a <- separate_about(x, footnotes="text")
    print(a$result)
    cat(a$about, sep="\n")
    invisible(x)
  }
}
