# Format p-values

Format p-values

## Usage

``` r
format_pvalue(
  p,
  digits = 2,
  max.digits = 4,
  justify = TRUE,
  addp = FALSE,
  na = ""
)

fmt_pvalue(data, columns = any_of(c("p.value", "p.adjust")), ...)
```

## Arguments

- p:

  a vector of p-value(s) to format.

- digits:

  the desired number of significant figures.

- max.digits:

  the maximum number of decimal places.

- justify:

  logical specifying whether or not to align by decimal point.

- addp:

  logical specifying whether `p =` or `p <` should be added to the
  output.

- na:

  value to replace missing values with (defaults to a blank).

- data:

  data set with columns to format.

- columns:

  desired columns to format.

- ...:

  additional parameters, sent to format_pvalue
