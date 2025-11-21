# Correlation Tests and Confidence Intervals

Correlation Tests and Confidence Intervals

## Usage

``` r
correlation_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  method = c("pearson", "kendall", "spearman"),
  conf.level = 0.95,
  ...
)
```

## Arguments

- formula:

  A formula of the form `y~x`

- data:

  A data frame containing the values in the formula

- alternative:

  options

- method:

  options

- conf.level:

  The desired confidence level for the confidence intervals.

- ...:

  Additional parameters
