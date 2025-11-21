# One-sample Wilcoxon test

Perform Wilcoxon's signed-rank test on a single sample

## Usage

``` r
one_wilcoxon_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  null = 0,
  conf.level = 0.95
)
```

## Arguments

- formula:

  a formula of the form `~ y` or `y ~ 1`, where `y` is a numeric
  variable. To perform test within subgroups, use `y ~ x` or
  `y ~ 1 | g`, or even `y ~ x | g`, where `x` and `g` are factor
  variables.

- data:

  a data frame containing the values in the formula.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- null:

  a number specifying the null proportion for testing a null hypothesis;
  if not specified, a null of a center at 0 is used.

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.
