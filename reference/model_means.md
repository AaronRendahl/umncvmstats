# Get model means and slopes (trends)

Retrieve estimated marginal means and slopes (trends) as computed by
[`emmeans::emmeans`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
and
[`emmeans::emtrends`](https://rvlenth.github.io/emmeans/reference/emtrends.html),
and tests for all pairwise differences, as computed by
[`emmeans::pairs.emmGrid`](https://rvlenth.github.io/emmeans/reference/contrast.html).

## Usage

``` r
model_means(
  model,
  formula,
  cld,
  backtransform = TRUE,
  type = if (isTRUE(backtransform)) "response" else "linear",
  ...
)

pairwise_model_means(
  model,
  formula,
  backtransform = TRUE,
  type = if (isTRUE(backtransform)) "response" else "linear",
  ...
)

model_slopes(model, formula, ..., cld = TRUE)

pairwise_model_slopes(model, formula, ...)
```

## Arguments

- model:

  a linear model or generalized linear model.

- formula:

  the desired means or slopes; see Details.

- cld:

  a logical variable specifying if a compact letter display should be
  used for pairwise comparisons between groups. Defaults to TRUE unless
  the desired means are for a numerical variable or there is only one
  mean reported.

- backtransform:

  if a linear model and response variable is of form `log(y)`, or the
  model is a logistic model , backtransform the resulting estimate and
  confidence interval bounds, so that they report either geometric means
  on the original scale and ratios (for log-transformed responses) or
  proportions and odds ratios (for logistic models).

- type:

  specify method of backtransformation to `emmeans`. If used,
  `backtransform` is ignored.

- ...:

  additional parameters, passed to `emmeans` or `emtrends`. See Details.

## Details

For means, specify the desired combinations of explanatory variables
using formula notation. For example, to get the means at all levels of a
variable `x`, use `~ x`. To get the means at all combinations of `x1`
and `x2`, use `~ x1 + x2.`

For slopes, specify the desired slope on the left side of the formula.
For example, to get the slopes for a variable `x` at each level of a
variable `g`, use `x ~ g.` To get the overall slope, use `x ~ 1`.

Additionally, one can specify groupings by using a `|` within a formula.
For example, to get the means of all combinations of `x1` and `x2`
grouped by each value of `x2`, use `~ x1 | x2`. This is especially
useful for getting pairwise tests of means and trends within subgroups.

By default, estimated marginal means and trends are estimated by
computing the average mean or trend across all values of any other
categorical explanatory variables equally, and at the mean value of any
other numeric explanatory variables.

One of most useful additional parameters that can be passed to `emmeans`
is `at`, which allows one to specify specific values of continuous
variables to compute means and/or trends at. For example, if one had a
continuous variable `x` in the model, one could compute the means at
`x=10` and `x=20` using `at = list(x = c(10, 20)`.
