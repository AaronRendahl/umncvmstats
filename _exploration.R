## TODO

## document set_digits
## make an "about" data to describe all the variables?
## add a cross-tabs/count function
## standard language in footnote, especially about % CI?
## "response" and "emmean" in column names of model output? also ".trend"?
## don't do cld if only two groups

## rename geom_beeswarm?
## "inference" instead of "test"

## default pch=21, fill="white" for geom_beeswarm?
## better bar plot axis spacing and ticks?
## log ticks (see below)

## test_by doesn't work unless package is loaded!
## umncvmstats::one_t_test(mpg~wt, data=mtcars)


## FIXED
## when one_proportion_test(y~x), x doesn't stay in factor order
## allow one_proportion_test(y ~ 1 + x)
## when one_proportion_test(y~x), what about missing x?
## error with boolean grouping
## in descriptive stats, min/max give warning and report Inf if all missing

## digits by SE when SE=0 (ie, proportion of 0/Na)
## this may be an example?
## combine_tests(
##     one_proportion_test(cyl ~ vs, data = mtcars2, all_success = TRUE),
##     independence_test(cyl ~ vs, data = mtcars2)) |> set_digits()

## n_sigfig = 2 rounds 143 to 140, don't like that...

x <- c(12345, 1234.5, 123.45, 12.345, 1.2345, 0.12345, 0.09999)
p <- 3

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



decimals_for(1234, digits=2)
decimals_for(9.899, digits=3)
decimals_for(0.02241)


x <- c(3.456, 45.56678, 3.4, 0, 234234)
formatC(x, digits=3, format="f")
formatC(x, digits=3, format="e")

library(gt)
a <- combine_tests(
  one_proportion_test(vs~1, mtcars),
  one_proportion_test(2,100),
  one_proportion_test(3,15))

a |> as_tibble(footnotes="below") |> mutate(proportion=sprintf("%0.2f", proportion)) |>
  writexl::write_xlsx("_temp.xlsx")
a |> as_tibble()

aa <- a |> as_gt()

dd <- sapply(a$SE, decimals_for, digits=2)
for(idx in seq_along(dd)) {
  aa <- aa |> fmt_numbers(c("proportion", "SE", "conf.low", "conf.high"), decimals=dd[idx], rows=idx)
}
aa2 <- aa |> fmt_numbers("proportion", decimals=4)

a |> as_gt() |>
  fmt_numbers(c("proportion", "SE", "conf.low", "conf.high"), decimals=3, rows=1) |>
  fmt_numbers(c("proportion", "SE", "conf.low", "conf.high"), decimals=2, rows=2)

##################################
ggplot(mtcars2) + aes(gear, fill=cyl) +
  geom_bars(position=position_dodge(preserve="single")) +
  scale_y_continuous(expand = expansion(mult=c(0, 0.05)),
                     breaks = seq(0, 12, by=4)) +
  #coord_flip() +
  scale_fill_discrete(name="Cylinders", type=c('6'="red", '4'="blue", '8'="green")) +
  theme(panel.grid.major.x = element_blank(),
        axis.ticks.length.x = unit(0, "pt"))



# log ticks
# https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-axes/

# https://github.com/tidyverse/ggplot2/pull/5777
ggplot(ggplot2::mpg, aes(class, fill = factor(cyl))) +
  geom_bar(position = position_dodge(preserve = "single"))# +
scale_x_discrete(
  minor_breaks = scales::breaks_width(1, 0.5),
  guide = guide_axis(minor.ticks = TRUE)
) +
  theme(
    panel.grid.major.x = element_blank(),
    axis.minor.ticks.length.x.bottom = unit(11, "pt"),
    axis.ticks.length.x.bottom = unit(0, "pt"),
    axis.text.x.bottom = element_text(margin = margin(t = -8.75))
  )



###################
devtools::load_all()
winsorize <- function(x, cutoff=0.05) {
  x |> pmax(quantile(x, cutoff, na.rm=TRUE)) |>
    pmin(quantile(x, 1-cutoff, na.rm=TRUE))
}


d <- readxl::read_excel("~/Sandbox/JoeHerbert_2024-08-02/JoeHerbert_2024-08-16.xlsx")
d2 <- d
vs <- c("Ph", "pCO2", "pO2", "ctHb", "K", "Na", "Cl", "Lac", "Base_excess",
        "HCO3", "CREA", "PCV", "TS")
for(v in vs) d2 <- d2 |> mutate(across(all_of(v), winsorize), .by=Species)
dF <- filter(d2, Species=="Feline")
dC <- filter(d2, Species=="Canine")

paste(vs, collapse=" + ")
mF <- glm(Survival ~
            Ph + pCO2 + log10(pO2) + ctHb +
            K + Na + Cl +
            log10(Lac) + Base_excess + HCO3 +
            log10(CREA) + PCV + TS,
          data=dF, family="binomial")
model_coefs(mF)


############
aa <- combine_tests(
  one_proportion_test(vs ~ 1 + am, mtcars2),
  two_proportion_test(vs ~ am, mtcars2),
  one_proportion_test(1, 1000),
  one_proportion_test(999, 1000),
  one_proportion_test(1, 1e6))
a1 <- aa |> set_digits(3L, columns=c(attr(aa, "estimate.vars"), "SE", "conf.low", "conf.high"))
aa |> set_digits_by(3L)
aa |> set_digits_by(2L)
aa |> set_digits_by(2L, by_row=FALSE)
aa[1:2,]

a1 <- aa[1:2,] |> set_digits_by(5)
a2 <- aa[3:4,] |> set_digits_by(1)
combine_tests(a1, a2)
