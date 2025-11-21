# One-sample proportion inference

Compute the proportion of "successes" in a sample and the corresponding
confidence interval. Optionally, compute a p-value for a specified null
hypothesis.

## Usage

``` r
# S3 method for class 'formula'
one_proportion_inference(
  formula,
  data,
  success,
  all_success = FALSE,
  method = c("default", "wilson", "exact"),
  correct = FALSE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  null = NULL,
  ...
)

# Default S3 method
one_proportion_inference(
  x,
  n,
  method = c("default", "wilson", "exact"),
  correct = FALSE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  null = NULL,
  ...
)

one_proportion_inference(x, ...)

wilson_test(
  x,
  n,
  null,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  correct = FALSE
)

binomial_test(
  x,
  n,
  null,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95
)
```

## Arguments

- formula:

  a formula of the form `~ y` or `y ~ 1`, where `y` is a factor
  variable. If not a factor, it will be automatically converted. To
  perform test within subgroups, use `y ~ x` or `y ~ 1 | g`, or even
  `y ~ x | g`.

- data:

  a data frame containing the values in the formula.

- success:

  an optional vector specifying the level(s) for which proportions
  should be reported.

- all_success:

  if TRUE, then proportions for all levels are reported.

- method:

  character string specifying which method to use. One of "`default`",
  "`wilson`", or "`exact`".

- correct:

  a logical indicating whether Yates' continuity correction should be
  applied; used for Wilson test only.

- alternative:

  character string specifying the alternative hypothesis, must be one of
  "`two.sided`" (default), "`greater`" or "`less`".

- conf.level:

  confidence level of the returned confidence interval. Must be a single
  number between 0 and 1.

- null:

  a number specifying the null proportion for testing a null hypothesis;
  if not specified, no hypothesis test is performed.

- ...:

  additional arguments, currently unused.

- x:

  number of successes.

- n:

  number of trials.

## Value

A tibble with class `atest` containing columns as follows:

- x:

  count of successes

- n:

  sample size

- proportion:

  proportion of successes

- conf.low:

  lower confidence bound

- conf.high:

  upper confidence bound

- null:

  the specified null value (if specified)

- p.value:

  the p-value of the test (if null specified)

## Details

By default only the confidence interval for the proportion is reported,
a hypothesis test can also be performed by specifying the desired null
value.

Two methods are currently supposed, either Wilson's method (both with or
without a continuity correction) or the Clopper-Pearson "exact" method.
If no method is specified, the default method is Wilson's method without
continuity correction, however, the "exact" method is chosen if the
sample size is less than 10, the observed proportion is less than 0.10,
or the minimum expected count under the null is less than 5 (if a null
hypothesis is specified).

Wilson's method uses
[stats::prop.test](https://rdrr.io/r/stats/prop.test.html) and the exact
method uses
[stats::binom.test](https://rdrr.io/r/stats/binom.test.html).
