#' Format p-values
#'
#' @param p XX
#' @param digits XX
#' @param max.digits XX
#' @param justify XX
#' @param addp XX
#' @param na XX
#'
#' @return XX
#' @export
format_pvalue <- function(p, digits=2, max.digits=4, justify=TRUE, addp=FALSE, na="") {
  if(digits > max.digits) max.digits <- digits
  pr <- round(p, max.digits)
  pf <- formatC(pr, digits=digits, format="fg", flag="#") |> str_sub(1, max.digits+2)
  pad <- if(addp) "p\u00A0=\u00A0" else if(justify) "\u2007\u00A0" else ""
  lessthan <- if(addp) "p\u00A0<\u00A0" else "<\u00A0"
  min.value <- sprintf("%s0.%s1", lessthan, paste(rep(0, max.digits-1), collapse=""))
  p_fmt <- case_when(is.na(pr) ~ na,
                     pr < .Machine$double.eps*10 ~ min.value,
                     TRUE ~ paste0(pad, pf))
  attributes(p_fmt) <- attributes(unclass(p))
  return(p_fmt)
}

#' @param data XX
#' @param columns XX
#' @param ... Additional parameters, sent to [format_pvalue]
#'
#' @rdname format_pvalue
#' @export
fmt_pvalue <- function(data, columns=any_of(c("p.value", "p.adjust")), ...) {
  data |>
    fmt(columns={{columns}}, fns=\(p) format_pvalue(p, ...)) |>
    cols_align(align="left", columns={{columns}})
}

#' Format numbers
#'
#' @param data XX
#' @param columns XX
#' @param ... XX
#'
#' @export
fmt_numbers <- function(data,
                        columns =
                          where(is.double) &
                          !any_of(c("p.value", "p.adjust")),
                        ...) {
  data |> fmt_number(columns={{columns}}, ...)
}
