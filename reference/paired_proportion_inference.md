# Paired proportion test (McNemar's)

Perform McNemar's test for paired proportions, by computing the
proportion of those that switch from failure to success out of all those
that switch in either direction and performing a one-sample proportion
test with a null of 0.5.

## Usage

``` r
paired_proportion_inference(
  formula,
  data,
  success,
  method = c("default", "wilson", "exact"),
  alternative = c("two.sided", "less", "greater"),
  correct = FALSE,
  conf.level = 0.95
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

- success:

  the level of the response variable to consider a success

- method:

  character string specifying which method to use. One of "`default`",
  "`wilson`", or "`exact`".

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- correct:

  a logical indicating whether Yates' continuity correction should be
  applied; used for Wilson test only.

- conf.level:

  if desired, confidence level of the returned confidence interval. Must
  be a single number between 0 and 1.
