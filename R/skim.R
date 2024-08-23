#' Summarize (skim) the variables in a data set
#'
#' `skim()`, from the `skimr` package, provides a broad overview of a data frame.
#' It summarizes each type of variable (character, factor, numeric, etc.) in a way
#' that is appropriate for each. See [skimr::skim].
#'
#'
#' @param data The data frame to skim.
#' @param ... Columns to select for skimming.
#' @param .data_name The name to use for the data. Defaults to the same as data
#'
#' @importFrom skimr skim
#' @export
skim <- function(data, ..., .data_name = NULL) {
  if (is.null(.data_name)) {
    .data_name <- rlang::expr_label(substitute(data))
  }
  out <- skimr::skim(data, ..., .data_name=.data_name)
  class(out) <- c("askim_df", class(out))
  out
}

#' @importFrom skimr df_name
skimr_process_data_name <- function (object) {
  raw_name <- df_name(object)
  no_ticks <- gsub("`", "", raw_name)
  if (no_ticks %in% c(".", ".data")) {
    "Piped data"
  }
  else if (nchar(no_ticks) > 25) {
    paste0(substring(no_ticks, 1, 25), "...")
  }
  else {
    no_ticks
  }
}

#' @export
#' @importFrom knitr knit_print
#' @importFrom markdown mark_html
#' @importFrom htmltools HTML
print.askim_df <- function(x, viewer=TRUE, ...) {
  if(viewer) {
    b2 <- knit_print(x)
    b4 <- mark_html(b2, template=TRUE)
    print(HTML(b4), browse=TRUE)
  } else {
    s <- summary(x)
    cat(sprintf("Summary of %s: %d rows, %d columns\n", s$data_name, s$data_rows, s$data_cols))
    NextMethod(include_summary=FALSE)
  }
}

#' @export
knit_print.askim_df <- function(x, ...) {
  s <- summary(x)
  a1 <- sprintf("**Summary of %s:**\n\n%d rows, %d columns", s$data_name, s$data_rows, s$data_cols)
  a2 <- NextMethod(options=list(skimr_include_summary=FALSE))
  paste(a1, a2)
}
