capture_warnings <- function(expr, warnings=c(), replacement=warnings) {
  w <- NULL
  findwarnings <- function(w) {
    m <- match(w$message, warnings)
    m <- m[!is.na(m)]
    if(length(m) > 0) {
      w <<- replacement[m]
      invokeRestart("muffleWarning")
    }
  }
  result <- withCallingHandlers(expr, warning = findwarnings)
  list(result=result, warnings=w)
}

checkif2 <- function(x, require_two=TRUE) {
  if(is.logical(x)) {
    x <- factor(x, levels=c(FALSE, TRUE))
  } else if(!is.factor(x)) {
    x <- factor(x)
  }
  if(require_two) {
    if(nlevels(x) > 2) stop("More than two unique levels found.")
    if(nlevels(x) < 2) stop("Only one unique level found.")
  }
  if(length(x) == 0) stop("No non-missing values found.")
  x
}

rm_class <- function(x, class) { class(x) <- setdiff(class(x), class); x }
