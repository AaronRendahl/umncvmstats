## TODO

## n_sigfig = 2 rounds 143 to 140, don't like that...
## make an "about" data to describe all the variables?
## add a cross-tabs/count function
## standard language in footnote, especially about % CI?
## "response" and "emmean" in column names of model output? also ".trend"?

## rename geom_beeswarm?
## "inference" instead of "test"

## default pch=21, fill="white" for geom_beeswarm?
## better bar plot axis spacing and ticks?
## log ticks (see below)

x <- c(12345, 1234.5, 123.45, 12.345, 1.2345, 0.12345, 0.09999)
p <- 3

decimals_for <- function(x, digits=2) {
  n <- floor(log10(abs(x))) + 1 - digits
  nc <- nchar(as.character(round(x/10^n)))
  if(nc > digits) n <- n + 1
  pmax(-n, 0)
}

decimals_for(1234, digits=2)
decimals_for(9.899, digits=3)


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
aa

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
