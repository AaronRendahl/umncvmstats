# Power calculations for two sample t tests and confidence intervals

Power calculations for two sample t tests and confidence intervals

## Usage

``` r
two_t_power(
  n = NULL,
  delta = NULL,
  sd = 1,
  sig.level = 0.05,
  power = 0.8,
  tol = .Machine$double.eps^0.25
)

two_t_power_equiv(
  n = NULL,
  equiv = NULL,
  sd = 1,
  conf.level = 0.95,
  power = 0.8,
  tol = .Machine$double.eps^0.25
)

two_t_power_me(
  n = NULL,
  me = NULL,
  sd = 1,
  conf.level = 0.95,
  power = 0.8,
  tol = .Machine$double.eps^0.25
)
```

## Arguments

- n:

  number of observations (per group)

- delta:

  true difference between the parameter of interest and the null
  hypothesis

- sd:

  standard deviation (within group)

- sig.level:

  significance level (for delta)

- power:

  power of the test, or of having the given margin of error or smaller

- tol:

  numerical tolerance used in root finding, the default providing (at
  least) four significant digits.

- equiv:

  equivalence bounds

- conf.level:

  confidence level (for equivalence and margin of error)

- me:

  margin of error of the confidence interval
