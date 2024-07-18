capture_warnings <- function(expr, warnings=c(), replacement=warnings) {
  myw <- NULL
  findwarnings <- function(w) {
    m <- match(w$message, warnings)
    m <- m[!is.na(m)]
    if(length(m) > 0) {
      myw <<- c(myw, replacement[m])
      invokeRestart("muffleWarning")
    }
  }
  findmessages <- function(w) {
    myw <<- c(myw, w$message)
    invokeRestart("muffleMessage")
  }
  result <- withCallingHandlers(expr, warning = findwarnings, message=findmessages)
  list(result=result, warnings=myw)
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
