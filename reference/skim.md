# Summarize (skim) the variables in a data set

`skim()`, from the `skimr` package, provides a broad overview of a data
frame. It summarizes each type of variable (character, factor, numeric,
etc.) in a way that is appropriate for each. See
[skimr::skim](https://docs.ropensci.org/skimr/reference/skim.html).

## Usage

``` r
skim(data, ..., .data_name = NULL)
```

## Arguments

- data:

  The data frame to skim.

- ...:

  Columns to select for skimming.

- .data_name:

  The name to use for the data. Defaults to the same as data
