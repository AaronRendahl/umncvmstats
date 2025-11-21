# Set desired digits for values in an atest

Set desired digits for values in an atest

## Usage

``` r
set_digits(
  x,
  digits = 2,
  decimals,
  rows = seq_len(nrow(x)),
  columns = c("_estimate_", "SE", "conf.low", "conf.high", "predict.low", "predict.high"),
  by = NA,
  by_row = TRUE,
  overwrite = TRUE
)
```

## Arguments

- x:

  The atest object

- digits:

  The desired number of significant digits.

- decimals:

  Alternatively, the number of decimal places to show.

- rows:

  The rows to apply this to

- columns:

  The columns to apply this to; by default, the columns corresponding to
  the estimate, the SE, and the confidence intervals.

- by:

  The column that determines the significant digits, by default the SE.

- by_row:

  Whether or not to compute the significant digits by row (the default),
  or to use the number of digits such that all rows have at least this
  many.

- overwrite:

  Whether or not to overwrite an existing digits specification.
