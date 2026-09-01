# Compute descriptive statistics for a data set

Use
[`gtsummary::tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
to summarize variables in a data set. For categorical variables, reports
count/total and percent; for continuous variables, reports mean,
standard deviation, median, and range.

## Usage

``` r
descriptive_statistics(data, ..., digits = 2, compact = TRUE)
```

## Arguments

- data:

  the data set to summarize.

- ...:

  additional parameters, sent to `tbl_summary`.

- digits:

  desired number of significant figures, for numeric variables.

- compact:

  logical specifying whether or not to compact the resulting `gt` table.

## Examples

``` r
mtcars2 |> dplyr::select(-model) |> descriptive_statistics()


  

Characteristic
```
