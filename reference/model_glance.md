# Get model summary information

Retrieve statistics such as R-squared, AIC, and more, as computed by
[`broom::glance`](https://generics.r-lib.org/reference/glance.html).

## Usage

``` r
model_glance(model, ...)
```

## Arguments

- model:

  a linear model or generalized linear model.

- ...:

  additional parameters, passed to
  [`broom::glance`](https://generics.r-lib.org/reference/glance.html).
