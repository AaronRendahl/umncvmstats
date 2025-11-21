# Add a smooth from a logistic regression

Add a smooth to a `ggplot`, using a logistic model.

## Usage

``` r
geom_smooth_logistic(mapping = NULL, data = NULL, se = TRUE, ...)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [aes()](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  mapping if there is no plot mapping.

- data:

  The data to be displayed in this layer. See
  [`ggplot2::geom_smooth`](https://ggplot2.tidyverse.org/reference/geom_smooth.html)
  for additional details.

- se:

  logical specifying if confidence interval should be shown around the
  smooth.

- ...:

  other arguments passed to `geom_smooth`.

## Details

Uses `geom_smooth` with method `glm` and the `binomial` family, and a
formula of `factor(y) ~ x`.
