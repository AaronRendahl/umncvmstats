# Paired t-test

Compute the mean difference between paired samples, the corresponding
confidence interval, and a p-value for the null hypothesis of equal
means (that is, a difference of zero).

## Usage

``` r
paired_t_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  null = 0,
  conf.level = 0.95,
  backtransform = TRUE
)
```

## Arguments

- formula:

  a formula of the form `y2 - y1 ~ 1` or `~ y2 -y1`, where `y1` and `y2`
  are numeric variables. To perform test within subgroups, use
  `y2 - y1 ~ x` or `y2 - y1 ~ 1 | g`, or even `y2- y1 ~ x | g`, where
  `x` and `g` are factor variables.

- data:

  a data frame containing the values in the formula.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- null:

  a number specifying the null proportion for testing a null hypothesis;
  if not specified, no hypothesis test is performed.

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- backtransform:

  if response variable is of form `log(y2) - log(y1)`, backtransform the
  resulting estimate and confidence interval bounds, so that they report
  the ratio of the geometric means rather than the difference on the log
  scale.
