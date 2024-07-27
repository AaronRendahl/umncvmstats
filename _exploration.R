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
