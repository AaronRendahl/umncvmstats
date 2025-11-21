# Two-sample t-test

Compute the difference in means between two samples, the corresponding
confidence interval, and a p-value for the null hypothesis of equal
means (that is, a difference of zero).

## Usage

``` r
two_t_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  null = 0,
  var.equal = FALSE,
  conf.level = 0.95,
  conf.adjust = 1,
  backtransform = TRUE
)

pairwise_t_inference(
  formula,
  data,
  adjust = c("bonferroni", "holm", "none"),
  reverse = FALSE,
  ...
)
```

## Arguments

- formula:

  a formula of the form `y ~ x`, where `y` is a numeric variable and `x`
  is a factor variable. To perform test within subgroups, use
  `y ~ x | g`, where `x` and `g` are factor variables.

- data:

  a data frame containing the values in the formula.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- null:

  a number specifying the null proportion for testing a null hypothesis;
  if not specified, no hypothesis test is performed.

- var.equal:

  a logical variable indicating whether to treat the two variances as
  being equal. The default is to assume unequal variance.

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- conf.adjust:

  adjust confidence bounds for `conf.adjust` simultaneous intervals
  using the Bonferroni method. Used internally by
  `pairwise_t_inference`; should only rarely be used by users.

- backtransform:

  if response variable is of form `log(...)`, backtransform the
  resulting estimate and confidence interval bounds, so that they report
  the ratio of the geometric means rather than the difference on the log
  scale.

- adjust:

  method of adjusting p-values for multiple comparisons, one of
  "`bonferroni`", "`holm`", or "`none`".

- reverse:

  reverse the direction of pairwise comparisons.

- ...:

  further arguments to be passed to submethods, as appropriate.
