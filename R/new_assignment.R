#' Create a blank new assignment
#'
#' @param filename XX
#' @param title XX
#' @param author XX
#' @param open XX
#' @param template XX
#'
#' @return XX
#' @importFrom rlang is_interactive
#' @importFrom rstudioapi isAvailable
#' @importFrom rstudioapi navigateToFile
#' @export
new_assignment <- function(filename, title, author, open=is_interactive() & isAvailable(), template="template.qmd") {
  if(!str_detect(filename, "\\.qmd$")) {
    filename <- paste0(filename, ".qmd")
  }
  if(file.exists(filename)) {
    stop(sprintf("%s already exists.", filename))
  }
  if(str_detect(title, '"')) stop("Cannot use quotation marks in title.")
  if(str_detect(author, '"')) stop("Cannot use quotation marks in author.")
  f <- system.file(template, package="umncvmstats")
  if(f=="") {
    stop(sprintf("%s not found.", template))
  }
  a <- readLines(f) |>
    str_replace("^title:.*", sprintf('title: "%s"', title)) |>
    str_replace("^author:.*", sprintf('author: "%s"', author))
  cat(a, file=filename, sep="\n")
  if(open) navigateToFile(filename)
  invisible(filename)
}
