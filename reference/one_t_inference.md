# One-sample t-test

Compute the mean of a sample and the corresponding confidence interval,
using Student's t distribution. Optionally, compute a p-value for a
specified null hypothesis.

## Usage

``` r
one_t_inference(
  formula,
  data,
  alternative = c("two.sided", "less", "greater"),
  null,
  conf.level = 0.95,
  backtransform = TRUE
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
  if not specified, no hypothesis test is performed.

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- backtransform:

  if response variable is of form `log(...)`, backtransform the
  resulting estimate and confidence interval bounds.
