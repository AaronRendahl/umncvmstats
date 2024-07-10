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

fmt_pvalue <- function(data, columns=matches("p.value"), ...) {
  data |>
    fmt(columns={{columns}}, fns=\(p) format_pvalue(p, ...)) |>
    cols_align(align="left", columns={{columns}})
}

hide_y_axis <- function() {
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.length.y = unit(0, "pt"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank())
}

glm1 <- function(formula, data,...) {
  x <- match.call()
  x[[1]] <- quote(glm)
  out <- eval.parent(x)
  class(out) <- c("glm1", class(out))
  out
}

predictdf.glm1 <- function (model, xseq, se, level) {
  out <- ggplot2:::predictdf.glm(model, xseq, se, level)
  out$y <- out$y + 1
  if(se) {
    out$ymax <- out$ymax + 1
    out$ymin <- out$ymin + 1
  }
  out
}

geom_smooth_binomial <- function(...) {
  geom_smooth(aes(group=1),
              method=glm1,
              formula=factor(y) ~ x,
              method.args=list(family=binomial()))
}
