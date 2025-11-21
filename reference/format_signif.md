# Format to significant digits

Format to significant digits

## Usage

``` r
format_signif(x, digits = 3, max_small = 6, max_big = 6, keep_big = digits)
```

## Arguments

- x:

  the numbers to format

- digits:

  how many significant digits to show

- max_small:

  maximum number of decimals to show before using scientific notation

- max_big:

  maximum number of places to show (for large numbers) before using
  scientific notation

- keep_big:

  number of significant digits to show when using scientific notation
  for large numbers
