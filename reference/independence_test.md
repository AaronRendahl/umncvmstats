# Independence Tests for Count Data

Perform an independence test between two categorical variables, using
either the chi-squared method or Fisher's exact method.

## Usage

``` r
# S3 method for class 'formula'
independence_test(
  formula,
  data,
  method = c("default", "chisq", "exact"),
  correct = FALSE,
  ...
)

# Default S3 method
independence_test(
  x,
  method = c("default", "chisq", "exact"),
  correct = FALSE,
  ...
)

independence_test(x, ...)
```

## Arguments

- formula:

  a formula of the form `y ~ x`, where `y` and `x` are both factor
  variables. If not factors, they will be automatically converted. To
  perform test within subgroups, use `y ~ x | g`.

- data:

  a data frame containing the values in the formula.

- method:

  character string specifying which method to use. One of "`default`",
  "`chisq`", or "`exact`".

- correct:

  a logical indicating whether Yates' continuity correction should be
  applied; used for chi-squared test only.

- ...:

  additional parameters, currently unused.

- x:

  instead of a formula, provide the matrix of counts directly.

## Details

By default, the chi-squared method with no continuity correction is used
unless the minimum expected count under the null is less than 5, in
which case Fisher's exact test is used.
