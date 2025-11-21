# Additional Options

## Formatting `umncvmstats` Results

All output from statistical tests in this package are stored as a
special object (an `atest`) that knows how to print itself in a nice
way.

It does this by converting it into a `gt` object from the
[gt](https://gt.rstudio.com/) package.

You can use any `gt` function by first doing this conversion yourself
using `as_gt`. If you use any `gt` functions you’ll need to load the
`gt` library before `umncvmstats`.

The following code simply converts to a `gt` object, though has no
change from the default output because I haven’t added any other
functions yet.

``` r
a <- pairwise_t_inference(mpg ~ cyl, data=mtcars2)
a |> as_gt()
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.6       | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

### Compacting Results

The `tab_compact` function makes the table of results smaller; by
default it uses `font.size = 13` and `padding = 1`.

``` r
a |> as_gt() |> tab_compact()
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.6       | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

For comparison, here is `font.size = 11` and `padding = 3`.

``` r
a |> as_gt() |> tab_compact(font.size = 11, padding = 3)
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.6       | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

## Rounding `umncvmstats` Results

### By significant digits or decimals

It’s trickier than I think it should be to get R to output a reasonable
number of digits; this was one reason I wrote this package.

The formatted output from statistical inference functions in this
package makes a guess that should be reasonable in most, but not all,
cases.

To modify the number of digits that are output, use the `set_digits`
function. You can either specify the number of significant digits or the
number of decimal places. By default, digits for the estimate, the SE,
and the confidence bounds are set for all rows.

The `set_digits` function should be used directly on the results object,
before any `gt` formatting.

The code sets the number of significant digits to 2 (for the estimate,
the SE, and the confidence bounds),

``` r
a |> set_digits(digits=2)
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 11        | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 12         | 1.5  | 15.0 | 7.5      | 16        | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.6       | 0.88 | 18.5 | 2.3      |  7.0      | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

This code sets the number of decimal places to 2.

``` r
a |> set_digits(decimals=2)
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.92      | 1.47 | 13.0 | 2.89     | 10.95     | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.56      | 1.52 | 15.0 | 7.46     | 15.67     | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

Specific values can be specified using the column names and row numbers.

``` r
a |> set_digits(decimals=5, columns="difference", rows=1:2)
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.92078   | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.56364   | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

You can also set the number of decimals based on significant digits for
a given column or columns; for example, when created, the decimals are
set based on the number of significant digits needed for the SE and ME
columns, where ME (the margin of error) is calculated from conf.high and
conf.low.

This determines how many decimals are needed to display the SE and ME
with 2 significant digits, and uses that for the estimate, the SE, and
the confidence bounds. The following is the current default setting.

``` r
a |> set_digits(digits=2, by=c("SE", "ME"))
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   | p.adjust  |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|-----------|-----------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.0004  |   0.0012  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.6       | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.0001 | \< 0.0001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    | \< 0.0001 |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |           |           |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |           |           |

### Rounding p-values

Rounding p-values are currently handled within the `gt` mechanism, using
the `fmt_pvalue` function, which formats p-values (anything named
`p.value` or `p.adjust`), with `digits` significant digits, up to
`max.digits` decimals. The default is 2 significant digits, and 4
maximum digits.

Here’s what the output would look like with 3 significant digits and 5
maximum digits.

``` r
a |> as_gt() |> fmt_pvalue(digits=3, max.digits=5)
```

| response                                                                                                                    | variable   | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value    | p.adjust   |
|-----------------------------------------------------------------------------------------------------------------------------|------------|------------|------|------|----------|-----------|-------|---------|------------|------------|
| mpg                                                                                                                         | cyl: 4 - 6 |  6.9       | 1.5  | 13.0 | 2.9      | 10.9      | 0.000 | 4.72    |   0.00040  |   0.00121  |
| mpg                                                                                                                         | cyl: 4 - 8 | 11.6       | 1.5  | 15.0 | 7.5      | 15.7      | 0.000 | 7.60    | \< 0.00001 | \< 0.00001 |
| mpg                                                                                                                         | cyl: 6 - 8 |  4.64      | 0.88 | 18.5 | 2.33     |  6.95     | 0.000 | 5.29    |   0.00005  |   0.00014  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |            |      |      |          |           |       |         |            |            |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                   |            |            |      |      |          |           |       |         |            |            |

## Output as data set

To get the underlying output as a data set, use `as_tibble`. You’ll need
to have loaded the `tibble` library, which loads as part of the
`tidyverse`. There are several options for how to handle the footnotes.

This is useful when further processing or graphics based on output is
desired.

You’ll notice that the column names are different from the formatted
output; the formatted output tries to simplify the table to make it
clearer to read, while this instead returns the underlying pieces for
you to handle as needed in your specific case.

``` r
a |> as_tibble()
#> # A tibble: 3 × 13
#>   .y    .x    .x_contrast difference    SE    df conf.low conf.high  null
#>   <chr> <chr> <chr>            <dbl> <dbl> <dbl>    <dbl>     <dbl> <dbl>
#> 1 mpg   cyl   4 - 6             6.92 1.47   13.0     2.89     10.9      0
#> 2 mpg   cyl   4 - 8            11.6  1.52   15.0     7.46     15.7      0
#> 3 mpg   cyl   6 - 8             4.64 0.877  18.5     2.33      6.95     0
#> # ℹ 4 more variables: t.value <dbl>, p.value <dbl>, p.adjust <dbl>, about <chr>
```

## Multiple tests

In the one and two-sample tests (that is, when you’re not fitting a
model), you can use `+` to specify that the test should be done on
several variables.

``` r
one_t_inference(wt + mpg ~ am + vs, data = mtcars2)
```

| response                                                      | variable       | n   | mean  | SE   | df  | conf.low | conf.high |
|---------------------------------------------------------------|----------------|-----|-------|------|-----|----------|-----------|
| wt                                                            | am = automatic | 19  |  3.77 | 0.18 | 18  |  3.39    |  4.14     |
| wt                                                            | am = manual    | 13  |  2.41 | 0.17 | 12  |  2.04    |  2.78     |
| wt                                                            | vs = V-shaped  | 18  |  3.69 | 0.21 | 17  |  3.24    |  4.14     |
| wt                                                            | vs = straight  | 14  |  2.61 | 0.19 | 13  |  2.20    |  3.02     |
| mpg                                                           | am = automatic | 19  | 17.15 | 0.88 | 18  | 15.30    | 19.00     |
| mpg                                                           | am = manual    | 13  | 24.4  | 1.7  | 12  | 20.7     | 28.1      |
| mpg                                                           | vs = V-shaped  | 18  | 16.62 | 0.91 | 17  | 14.70    | 18.54     |
| mpg                                                           | vs = straight  | 14  | 24.6  | 1.4  | 13  | 21.5     | 27.7      |
| One Sample t-test (two.sided), with 95% confidence intervals. |                |     |       |      |     |          |           |

``` r
two_t_inference(wt + mpg ~ am + vs, data = mtcars2)
```

| response                                                            | variable                | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   |
|---------------------------------------------------------------------|-------------------------|------------|------|------|----------|-----------|-------|---------|-----------|
| wt                                                                  | am: automatic - manual  |  1.36      | 0.25 | 29.2 |   0.85   |  1.86     | 0.000 |  5.49   | \< 0.0001 |
| wt                                                                  | vs: V-shaped - straight |  1.08      | 0.29 | 30.0 |   0.49   |  1.66     | 0.000 |  3.76   |   0.0007  |
| mpg                                                                 | am: automatic - manual  | −7.2       | 1.9  | 18.3 | −11.3    | −3.2      | 0.000 | -3.77   |   0.0014  |
| mpg                                                                 | vs: V-shaped - straight | −7.9       | 1.7  | 22.7 | −11.5    | −4.4      | 0.000 | -4.67   |   0.0001  |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals. |                         |            |      |      |          |           |       |         |           |

You can also use `|` to split the data set into groups before running
the tests.

``` r
one_t_inference(wt + mpg ~ am | cyl, data = mtcars2)
```

| group                                                         | response | variable       | n   | mean   | SE    | df  | conf.low | conf.high |
|---------------------------------------------------------------|----------|----------------|-----|--------|-------|-----|----------|-----------|
| cyl = 4                                                       | wt       | am = automatic | 3   |  2.94  | 0.24  | 2   |  1.92    |  3.95     |
|                                                               | wt       | am = manual    | 8   |  2.04  | 0.14  | 7   |  1.70    |  2.38     |
|                                                               | mpg      | am = automatic | 3   | 22.90  | 0.84  | 2   | 19.29    | 26.51     |
|                                                               | mpg      | am = manual    | 8   | 28.1   | 1.6   | 7   | 24.3     | 31.8      |
| cyl = 6                                                       | wt       | am = automatic | 4   |  3.389 | 0.058 | 3   |  3.204   |  3.574    |
|                                                               | wt       | am = manual    | 3   |  2.755 | 0.074 | 2   |  2.437   |  3.073    |
|                                                               | mpg      | am = automatic | 4   | 19.12  | 0.82  | 3   | 16.53    | 21.72     |
|                                                               | mpg      | am = manual    | 3   | 20.57  | 0.43  | 2   | 18.70    | 22.43     |
| cyl = 8                                                       | wt       | am = automatic | 12  |  4.10  | 0.22  | 11  |  3.62    |  4.59     |
|                                                               | wt       | am = manual    | 2   |  3.37  | 0.20  | 1   |  0.83    |  5.91     |
|                                                               | mpg      | am = automatic | 12  | 15.05  | 0.80  | 11  | 13.29    | 16.81     |
|                                                               | mpg      | am = manual    | 2   | 15.40  | 0.40  | 1   | 10.32    | 20.48     |
| One Sample t-test (two.sided), with 95% confidence intervals. |          |                |     |        |       |     |          |           |

``` r
two_t_inference(wt + mpg ~ am | cyl, data = mtcars2)
```

| group                                                               | response | variable               | difference | SE    | df    | conf.low | conf.high | null  | t.value | p.value  |
|---------------------------------------------------------------------|----------|------------------------|------------|-------|-------|----------|-----------|-------|---------|----------|
| cyl = 4                                                             | wt       | am: automatic - manual |  0.89      | 0.28  |  3.65 |  0.10    |  1.69     | 0.000 |  3.23   |   0.036  |
|                                                                     | mpg      | am: automatic - manual | −5.2       | 1.8   |  9.00 | −9.2     | −1.1      | 0.000 | -2.89   |   0.018  |
| cyl = 6                                                             | wt       | am: automatic - manual |  0.634     | 0.094 |  4.17 |  0.377   |  0.891    | 0.000 |  6.74   |   0.0022 |
|                                                                     | mpg      | am: automatic - manual | −1.44      | 0.92  |  4.41 | −3.92    |  1.03     | 0.000 | -1.56   |   0.19   |
| cyl = 8                                                             | wt       | am: automatic - manual |  0.73      | 0.30  |  4.37 | −0.07    |  1.54     | 0.000 |  2.46   |   0.065  |
|                                                                     | mpg      | am: automatic - manual | −0.35      | 0.90  | 10.2  | −2.34    |  1.64     | 0.000 | -0.391  |   0.70   |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals. |          |                        |            |       |       |          |           |       |         |          |

## Combining output

You can use `combine_tests` to combine output from several tests
together. The package will do its best to output this meaningfully, but
it’s up to you to combine meaningful things together.

``` r
combine_tests(
  one_t_inference(wt ~ am|vs, data = mtcars2),
  two_t_inference(wt ~ am, data = mtcars2),
  correlation_inference(mpg ~ wt + hp, data=mtcars2),
  one_proportion_inference(vs ~ am|cyl, data=mtcars2)) |>
  as_gt() |> tab_compact()
```

[TABLE]
