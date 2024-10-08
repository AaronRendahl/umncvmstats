#' Format p-values
#'
#' @param p a vector of p-value(s) to format.
#' @param digits the desired number of significant figures.
#' @param max.digits the maximum number of decimal places.
#' @param justify logical specifying whether or not to align by decimal point.
#' @param addp logical specifying whether `p =` or `p <` should be added to the output.
#' @param na value to replace missing values with (defaults to a blank).
#'
#' @rdname format_pvalue
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

#' @param data data set with columns to format.
#' @param columns desired columns to format.
#' @param ... additional parameters, sent to format_pvalue
#' @rdname format_pvalue
#' @export
fmt_pvalue <- function(data, columns=any_of(c("p.value", "p.adjust")), ...) {
  data |>
    fmt(columns={{columns}}, fns=\(p) format_pvalue(p, ...) |> str_replace_all("<", "&lt;")) |>
    cols_align(align="left", columns={{columns}})
}
