# About Data

We’ll mostly use functions from the `tidyverse` when working with data;
for more on the `tidyverse`, see the following resources:

- [dplyr Reference](https://dplyr.tidyverse.org/)
- [R for Data Science book](https://r4ds.hadley.nz/)

## About data sets

A good data file …

- Is rectangular
- Has observations in rows and variables in columns
- Can have extra information (metadata) in rows above the data; we can
  have R skip these
- Has no information in formatting (ie, color) or position (ie, sorting
  shouldn’t lose information)
- Has minimal computation or duplication
- Has well-labeled variables…

Unfortunately computers and humans have different ideas about what a
good label is.

- Good computer labels: Alphanumeric and underscore only, starting with
  a letter. Units often get in the way. (eg, `BloodPressure` or
  `blood_pressure`)
- Good human labels: spaces, parentheses allowed, units should be
  specified. “Blood Pressure (mmHg)”

Consider using two rows for labels, with human labels (to be skipped
when reading data in) above computer labels.

## Reading in data

We’ll most often read in data from either csv or Excel sheets. The
functions `read_csv` and `read_excel` are used for this.

Both take the filename as the first parameter, and have additional
parameters `na` to specify what values should be considered missing, and
`skip` to skip a certain number of rows at the top. For an Excel sheet,
there’s also the optional `sheet` parameter to specify which sheet to
read; by default the first sheet is read.

It’s also recommended to use the `here` function to specify the location
of the file within your project; this allows you to move your script or
Quarto file to subfolders within the project without changing the code.

For example, the following code reads in a csv file `test.csv` from the
main folder, considers a single period `.` and the code `NA` to be
missing values, and skips the first two lines. It also saves the result
in the variable `testdata`.

``` r
testdata <- read_csv(here("test.csv"), na = c(".", "NA"), skip = 2)
```

Here’s similar for reading an Excel file, that specifically chooses to
read from the sheet named `mydata`. If not specified, the first sheet is
chosen.

``` r
testdata <- read_excel(here("test.csv"), sheet = "mydata", na = c(".", "NA"), skip = 2)
```

For further examples in this document, I’ll use the `mtcars2` data set,
which is included in the `umncvmstats` package.

### Checking that it was read properly

After first reading in a data file, use the `skim` function to be sure
it read in properly. This shows the number of rows and columns that were
read, and then for each type of variable (character/factor/numeric,
etc.), it shows some basic summary information about each variable.

This function is in `umncvmstats` and uses the `skimr` package behind
the scenes; further control is possible by using that package directly.

``` r
mtcars2 |> skim()
```

**Summary of mtcars2:**

32 rows, 12 columns

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| model         |         0 |             1 |   7 |  19 |     0 |       32 |          0 |

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts               |
|:--------------|----------:|--------------:|:--------|---------:|:-------------------------|
| cyl           |         0 |             1 | FALSE   |        3 | 8: 14, 4: 11, 6: 7       |
| vs            |         0 |             1 | FALSE   |        2 | V-s: 18, str: 14         |
| am            |         0 |             1 | FALSE   |        2 | aut: 19, man: 13         |
| gear          |         0 |             1 | FALSE   |        3 | 3: 15, 4: 12, 5: 5       |
| carb          |         0 |             1 | FALSE   |        6 | 2: 10, 4: 10, 1: 7, 3: 3 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |     sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:--------------|----------:|--------------:|-------:|-------:|------:|-------:|-------:|-------:|-------:|:------|
| mpg           |         0 |             1 |  20.09 |   6.03 | 10.40 |  15.43 |  19.20 |  22.80 |  33.90 | ▃▇▅▁▂ |
| disp          |         0 |             1 | 230.72 | 123.94 | 71.10 | 120.83 | 196.30 | 326.00 | 472.00 | ▇▃▃▃▂ |
| hp            |         0 |             1 | 146.69 |  68.56 | 52.00 |  96.50 | 123.00 | 180.00 | 335.00 | ▇▇▆▃▁ |
| drat          |         0 |             1 |   3.60 |   0.53 |  2.76 |   3.08 |   3.70 |   3.92 |   4.93 | ▇▃▇▅▁ |
| wt            |         0 |             1 |   3.22 |   0.98 |  1.51 |   2.58 |   3.33 |   3.61 |   5.42 | ▃▃▇▁▂ |
| qsec          |         0 |             1 |  17.85 |   1.79 | 14.50 |  16.89 |  17.71 |  18.90 |  22.90 | ▃▇▇▂▁ |

In R, a categorical variable is called a `factor`; text variables should
usually be factors unless they are simply used for identification, like
the `model` variable, here.

The most common error is that a variable that should be numeric is read
in as text, due to an unspecified missing value code. Be sure to check
that all variables are of the expected type, and that the values as
summarized make sense.

### Creating factors

The `mtcars2` data set is based on the `mtcars` data set, which is
included in your basic R installation. In the original `mtcars` data
set, though, all variables are numeric; the categorical variables are
all coded as 0/1.

Compare the `skim` summary of the `mtcars` data set with `mtcars2`:

``` r
mtcars |> skim()
```

**Summary of mtcars:**

32 rows, 11 columns

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |     sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:--------------|----------:|--------------:|-------:|-------:|------:|-------:|-------:|-------:|-------:|:------|
| mpg           |         0 |             1 |  20.09 |   6.03 | 10.40 |  15.43 |  19.20 |  22.80 |  33.90 | ▃▇▅▁▂ |
| cyl           |         0 |             1 |   6.19 |   1.79 |  4.00 |   4.00 |   6.00 |   8.00 |   8.00 | ▆▁▃▁▇ |
| disp          |         0 |             1 | 230.72 | 123.94 | 71.10 | 120.83 | 196.30 | 326.00 | 472.00 | ▇▃▃▃▂ |
| hp            |         0 |             1 | 146.69 |  68.56 | 52.00 |  96.50 | 123.00 | 180.00 | 335.00 | ▇▇▆▃▁ |
| drat          |         0 |             1 |   3.60 |   0.53 |  2.76 |   3.08 |   3.70 |   3.92 |   4.93 | ▇▃▇▅▁ |
| wt            |         0 |             1 |   3.22 |   0.98 |  1.51 |   2.58 |   3.33 |   3.61 |   5.42 | ▃▃▇▁▂ |
| qsec          |         0 |             1 |  17.85 |   1.79 | 14.50 |  16.89 |  17.71 |  18.90 |  22.90 | ▃▇▇▂▁ |
| vs            |         0 |             1 |   0.44 |   0.50 |  0.00 |   0.00 |   0.00 |   1.00 |   1.00 | ▇▁▁▁▆ |
| am            |         0 |             1 |   0.41 |   0.50 |  0.00 |   0.00 |   0.00 |   1.00 |   1.00 | ▇▁▁▁▆ |
| gear          |         0 |             1 |   3.69 |   0.74 |  3.00 |   3.00 |   4.00 |   4.00 |   5.00 | ▇▁▆▁▂ |
| carb          |         0 |             1 |   2.81 |   1.62 |  1.00 |   2.00 |   2.00 |   4.00 |   8.00 | ▇▂▅▁▁ |

To convert these variables into categorical variables, we use the
functions `mutate`, `as_factor`, and optionally `fct_recode`; we’ll see
more about them later, for now, let’s just see a couple examples of how
we use them to convert these variables.

- `mutate` function creates new variables (possibly overwriting old
  ones). Within `mutate` we use `newname = [operation]` to perform an
  operation and assign the result to the variable `newname`. Multiple
  variables can be created within one call to `mutate` by using commas.
- `as_factor` converts a variable to a categorical variable. You may
  also see the original version, `factor`; this version is from the
  `tidyverse` and is preferred for improved consistency.
- `fct_recode` changes the coding of the values.

As an example, the following code:

- will save the result into a new variable `my_mtcars`
- pipes the `mtcars` dataset into the `mutate` function, then within
  mutate,
  - creates a new variable `cyl` (which will overwrite the previous
    `cyl`) by running `as_factor` on the `cyl` variable
  - creates a new variable `vs` (which again, overwrites the previous
    `vs`) by first running `as_factor` on `vs` and then recoding it so
    that `0` becomes `V-shaped` and `1` becomes `Straight`. Quotation
    marks are recommended, though in certain cases are not absolutely
    necessary.

We then `skim` the new result.

``` r
my_mtcars <- mtcars |>
  mutate(cyl = as_factor(cyl),
         vs = as_factor(vs) |> fct_recode("V-shaped"="0", "Straight"="1"))
skim(my_mtcars)
```

**Summary of my_mtcars:**

32 rows, 11 columns

**Variable type: factor**

| skim_variable | n_missing | complete_rate | ordered | n_unique | top_counts         |
|:--------------|----------:|--------------:|:--------|---------:|:-------------------|
| cyl           |         0 |             1 | FALSE   |        3 | 8: 14, 4: 11, 6: 7 |
| vs            |         0 |             1 | FALSE   |        2 | V-s: 18, Str: 14   |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |   mean |     sd |    p0 |    p25 |    p50 |    p75 |   p100 | hist  |
|:--------------|----------:|--------------:|-------:|-------:|------:|-------:|-------:|-------:|-------:|:------|
| mpg           |         0 |             1 |  20.09 |   6.03 | 10.40 |  15.43 |  19.20 |  22.80 |  33.90 | ▃▇▅▁▂ |
| disp          |         0 |             1 | 230.72 | 123.94 | 71.10 | 120.83 | 196.30 | 326.00 | 472.00 | ▇▃▃▃▂ |
| hp            |         0 |             1 | 146.69 |  68.56 | 52.00 |  96.50 | 123.00 | 180.00 | 335.00 | ▇▇▆▃▁ |
| drat          |         0 |             1 |   3.60 |   0.53 |  2.76 |   3.08 |   3.70 |   3.92 |   4.93 | ▇▃▇▅▁ |
| wt            |         0 |             1 |   3.22 |   0.98 |  1.51 |   2.58 |   3.33 |   3.61 |   5.42 | ▃▃▇▁▂ |
| qsec          |         0 |             1 |  17.85 |   1.79 | 14.50 |  16.89 |  17.71 |  18.90 |  22.90 | ▃▇▇▂▁ |
| am            |         0 |             1 |   0.41 |   0.50 |  0.00 |   0.00 |   0.00 |   1.00 |   1.00 | ▇▁▁▁▆ |
| gear          |         0 |             1 |   3.69 |   0.74 |  3.00 |   3.00 |   4.00 |   4.00 |   5.00 | ▇▁▆▁▂ |
| carb          |         0 |             1 |   2.81 |   1.62 |  1.00 |   2.00 |   2.00 |   4.00 |   8.00 | ▇▂▅▁▁ |

## Summarizing data

### Descriptive statistics for all variables

To get basic descriptive statistics, use the `descriptive_statistics`
function, which shows mean/SD/median/range for continuous variables and
count/percent for categorical variables (or continuous variables with
just a few values).

Here I’m using `select` (to be seen later) to remove the `model`
variable before computing these statistics.

This is a version of `tbl_summary` from the `gtsummary` package; further
control is possible by using that package directly.

``` r
mtcars2 |> select(-model) |> descriptive_statistics()
```

[TABLE]

One can also compute these statistics on subsets of the data, by
splitting it by another variable first, using the `by` parameter. This
code gets descriptive statistics for each `am` group (automatic and
manual) separately.

\[Output not shown\]

``` r
mtcars2 |> select(-model) |> descriptive_statistics(by=am)
```

This output is formatted, making it great for understanding and display,
but hard to work with further. To work with further, we’d need to
compute these values ourselves.

### Summarizing categorical variables

Use `count` to count categorical variables; count combinations of
multiple variables by separating with a comma.

``` r
mtcars2 |> count(vs)
#> # A tibble: 2 × 2
#>   vs           n
#>   <fct>    <int>
#> 1 V-shaped    18
#> 2 straight    14
mtcars2 |> count(vs, am)
#> # A tibble: 4 × 3
#>   vs       am            n
#>   <fct>    <fct>     <int>
#> 1 V-shaped automatic    12
#> 2 V-shaped manual        6
#> 3 straight automatic     7
#> 4 straight manual        7
```

We can then use `mutate` to make a new variable with the percent; see
more on `mutate` later. This adds columns with the overall percent for
each desired combination of variables. Note that within each
computation, the percents sum to 100%.

``` r
mtcars2 |> count(vs) |> mutate(percent = n / sum(n))
#> # A tibble: 2 × 3
#>   vs           n percent
#>   <fct>    <int>   <dbl>
#> 1 V-shaped    18   0.562
#> 2 straight    14   0.438
mtcars2 |> count(vs, am) |> mutate(percent = n / sum(n))
#> # A tibble: 4 × 4
#>   vs       am            n percent
#>   <fct>    <fct>     <int>   <dbl>
#> 1 V-shaped automatic    12   0.375
#> 2 V-shaped manual        6   0.188
#> 3 straight automatic     7   0.219
#> 4 straight manual        7   0.219
```

If, however, we want to compute the percent of one variable within
categories of another variable, we can use the `.by` parameter, which
does the computation separately for each value of the specified
variable. Now the percents within each value of `am` (automatic or
manual) sum to 100%.

``` r
mtcars2 |> count(vs, am) |> mutate(percent = n / sum(n), .by=am)
#> # A tibble: 4 × 4
#>   vs       am            n percent
#>   <fct>    <fct>     <int>   <dbl>
#> 1 V-shaped automatic    12   0.632
#> 2 V-shaped manual        6   0.462
#> 3 straight automatic     7   0.368
#> 4 straight manual        7   0.538
```

Another option is to use `xtabs` to get a “cross-tabulation” of multiple
variables. This uses the “formula” syntax, which we’ll see more of when
we do inference. It also stores the result in a matrix, which is easier
to view but harder to work with. The row and columns sums can be added
using `addmargins`.

``` r
xtabs(~vs + am, data=mtcars2)
#>           am
#> vs         automatic manual
#>   V-shaped        12      6
#>   straight         7      7
xtabs(~vs + am, data=mtcars2) |> addmargins()
#>           am
#> vs         automatic manual Sum
#>   V-shaped        12      6  18
#>   straight         7      7  14
#>   Sum             19     13  32
```

### Summarizing continuous variables

To get summary statistics for a continuous variable, use `summarize`.
Within `summarize`, use `newname = [operation]` to perform an operation
and assign the result to the variable. To do this separately for each
value of a categorical variable, use the `.by` parameter. Note the `.`
in front of `.by`; that is used here to make conflicts with your desired
name less likely.

``` r
mtcars2 |> summarize(mean_mpg=mean(mpg), sd_mpg=sd(mpg))
#> # A tibble: 1 × 2
#>   mean_mpg sd_mpg
#>      <dbl>  <dbl>
#> 1     20.1   6.03
mtcars2 |> summarize(mean_mpg=mean(mpg), sd_mpg=sd(mpg), .by=am)
#> # A tibble: 2 × 3
#>   am        mean_mpg sd_mpg
#>   <fct>        <dbl>  <dbl>
#> 1 manual        24.4   6.17
#> 2 automatic     17.1   3.83
```

See list of useful summary functions below.

## Working with data

### Selecting rows or columns

- Selecting columns (`select`)
  - Select `model` and `mpg` columns: `mtcars2 |> select(model, mpg)`
  - Select all but `model` and `mpg` columns:
    `mtcars2 |> select(-model, -mpg)`
- Selecting rows (`filter`)
  - Select rows that have manual transmissions:
    `mtcars2 |> filter(am=="manual")`
  - Select rows that have mpg \> 25: `mtcars2 |> filter(mpg > 25)`
  - See “Comparison” below for more options

### Sorting

Use `arrange` to sort

- Sort by increasing mpg: `mtcars2 |> arrange(mpg)`
- Sort by decreasing mpg: `mtcars2 |> arrange(desc(mpg))`

### Creating new variables

We use the `mutate` function to add a variable.

- `mutate` function creates new variables (possibly overwriting old
  ones). Within `mutate` we use `newname = [operation]` to perform an
  operation and assign the result to the variable

Best practice is that variables use only letters, numbers, and the
underscore (`_`), and start with a letter.

This data set has fuel economy in miles per gallon (`mpg`); suppose we
instead want to use the UK standard of liters/100 kilometers.

``` r
mtcars2 <- mtcars2 |> 
  mutate(Lp100km = 3.78541 / 1.60934 * 100 / mpg)
mtcars2 |> select(mpg, Lp100km) |> skim()
```

**Summary of select(mtcars2, mpg, Lp10…:**

32 rows, 2 columns

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |  mean |   sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
|:--------------|----------:|--------------:|------:|-----:|------:|------:|------:|------:|------:|:------|
| mpg           |         0 |             1 | 20.09 | 6.03 | 10.40 | 15.43 | 19.20 | 22.80 | 33.90 | ▃▇▅▁▂ |
| Lp100km       |         0 |             1 | 12.76 | 3.86 |  6.94 | 10.32 | 12.25 | 15.25 | 22.62 | ▅▇▆▁▁ |

In addition to arithmetic, there are a number of useful function for
creating new variables, as seen below.

## Useful functions

### For summarizing

Useful summary functions include:

- `mean`: mean
- `sd`: standard deviation
- `var`: variance
- `median`: median
- `quantile`: quantile (or percentile); use the `probs` parameter to
  specify which. For example, `quantile(x, probs=0.25)` gets the 25th
  percentile of the variable `x`.
- `min`: minimum value
- `max`: maximum value
- `IQR`: interquartile range
- `n()`: to get the number of observations (including when grouped using
  `.by`)

R is very careful with missing values and will not compute these if any
of the values are missing; this is a good thing as it forces you to
specifically handle it. If you want to remove any missing values before
computing these statistics, set the parameter `na.rm = TRUE` for any of
these functions.

### For comparing

- less than/greater to: `<`, `<=`, `>`, `>=`,
- equal to `==`, not equal to `!=` (note the double equals to test
  equality)
- `%in%`: tests if a value is in a vector, for example
  - For example, for a variable `v <- c("a", "b", "c")`, the operation
    `v %in% c("a", "b")` returns the vector `c(TRUE, TRUE, FALSE)`.
- Boolean operators: or `|`, and `&`, not `!`
- to test if a value is missing, use `is.na`; using `==NA` does not
  work. This is also vectorized; try `is.na(c(1,2,NA,4))`

### For creating new variables

- arithmetic functions (`+`, `-`, `*`, `/`)
- log functions; natural log is default, but base 10 and base 2 also can
  be used: `log`, `log10`, `log2`
- For creating new variables based on conditions:
  - For a binary condition: `if_else`
  - For several possible conditions: `case_when`
  - To break a numeric variable into categories: `cut`

### For factors

- `as_factor`: creates a factor
- `fct_recode`: recodes the levels to different values
- `fct_relevel`: reorders the factor levels by hand
- `fct_infreq`, `fct_reorder`: reorders the factor levels either by
  frequency or by sorting along another variable
- `droplevels`: removes levels that have zero values
