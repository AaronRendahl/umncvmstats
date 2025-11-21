# Get model coefficients

Retrieve coefficients and corresponding confidence intervals and
p-values, as computed by
[`broom::tidy`](https://generics.r-lib.org/reference/tidy.html).

## Usage

``` r
model_coefs(model, ...)
```

## Arguments

- model:

  a linear model or generalized linear model.

- ...:

  additional parameters, passed to
  [`broom::tidy`](https://generics.r-lib.org/reference/tidy.html).
