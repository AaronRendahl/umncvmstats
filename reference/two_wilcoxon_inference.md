# Two-sample Wilcoxon test

Perform the Wilcoxon rank sum to compare two groups

## Usage

``` r
two_wilcoxon_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  null = 0,
  conf.level = 0.95,
  conf.adjust = 1
)

pairwise_wilcoxon_inference(
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

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- conf.adjust:

  adjust confidence bounds for `conf.adjust` simultaneous intervals
  using the Bonferroni method. Used internally by
  `pairwise_wilcox_inference`; should only rarely be used by users.

- adjust:

  method of adjusting p-values for multiple comparisons, one of
  "`bonferroni`", "`holm`", or "`none`".

- reverse:

  reverse the direction of pairwise comparisons.

- ...:

  further arguments to be passed to submethods, as appropriate.
