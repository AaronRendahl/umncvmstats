# Two-sample proportion inference

Compute the absolute difference in proportion between two samples, the
corresponding confidence interval, and a p-value for the null hypothesis
of equal proportions (that is, a difference of zero).

## Usage

``` r
# S3 method for class 'formula'
two_proportion_inference(
  formula,
  data,
  success,
  method = c("default", "chisq", "exact"),
  correct = TRUE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  ...
)

# Default S3 method
two_proportion_inference(
  x,
  n,
  method = c("default", "chisq", "exact"),
  correct = TRUE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  conf.adjust = 1,
  ...
)

two_proportion_inference(x, ...)

pairwise_proportion_inference(
  formula,
  data,
  adjust = c("bonferroni", "holm", "none"),
  reverse = FALSE,
  ...
)
```

## Arguments

- formula:

  a formula of the form `y ~ x`, where `y` and `x` are both factor
  variables. If not factors, they will be automatically converted. To
  perform test within subgroups, use `y ~ x | g`.

- data:

  a data frame containing the values in the formula.

- success:

  an optional value specifying the level for which proportions should be
  reported.

- method:

  character string specifying which method to use. One of "`default`",
  "`chisq`", or "`exact`".

- correct:

  a logical indicating whether continuity correction should be applied;
  used for chi-squared test only.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- ...:

  further arguments to be passed to submethods, as appropriate.

- x:

  vector with count of successes in the two groups, or a 2x2 matrix with
  the counts.

- n:

  vector with count of total trials in the two groups.

- conf.adjust:

  adjust confidence bounds for `conf.adjust` simultaneous intervals
  using the Bonferroni method. Used internally by
  `pairwise_proportion_inference`; should only rarely be used by users.

- adjust:

  method of adjusting p-values for multiple comparisons, one of
  "`bonferroni`", "`holm`", or "`none`".

- reverse:

  reverse the direction of pairwise comparisons.

## Value

A tibble with class `atest` containing columns as follows:

- difference:

  the difference in proportion between the two groups

- conf.low:

  lower confidence bound

- conf.high:

  upper confidence bound

- chisq.value:

  the chi-squared value (if chi-squared method used)

- p.value:

  the p-value of the test

## Details

Two methods are currently supported, the asymptotic test using the
chi-squared statistic (either with or without continuity correction) or
Fisher's exact test. By default, the chi-squared method is used, with
continuity correction determined as in the
[stats::prop.test](https://rdrr.io/r/stats/prop.test.html) function,
unless the minimum expected count under the null of equal proportions is
less than 5, in which case Fisher's exact test is used.

When Fisher's exact test is used, no confidence interval for the
difference in proportions is reported.

The chi-squared option uses
[stats::prop.test](https://rdrr.io/r/stats/prop.test.html) and the exact
test option uses
[stats::fisher.test](https://rdrr.io/r/stats/fisher.test.html).
