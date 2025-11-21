# Get model Anova table

Retrieve Anova (analysis of variance) table for a linear model, or
analysis of deviance table for a generalized linear model, as computed
by [`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html).

## Usage

``` r
model_anova(model, ...)
```

## Arguments

- model:

  a linear model or generalized linear model.

- ...:

  additional parameters, passed to
  [`car::Anova`](https://rdrr.io/pkg/car/man/Anova.html).
