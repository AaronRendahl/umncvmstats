# Create a random scatterplot for guessing correlations

Create a random scatterplot for guessing correlations

## Usage

``` r
guess_cor(
  seed = NA,
  nonp = FALSE,
  skew = nonp,
  rank = FALSE,
  answer = c("interactive", "show", "hide"),
  return.data = FALSE
)
```

## Arguments

- seed:

  the random seed used in creating the data.

- nonp:

  should non-parametric correlations be shown?

- skew:

  should extra skew be randomly added?

- rank:

  should the data set be converted to ranks before plotting and
  computing correlations?

- answer:

  option to control interactivity; options are "`interactive`",
  "`show`", and "`hide`".

- return.data:

  normally the plot is returned; if TRUE, the data is returned instead.
