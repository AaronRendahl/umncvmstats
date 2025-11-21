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

**N = 32**

mpg

  

    Mean ± SD

20.09 ± 6.03

    Median (Min, Max)

19.20 (10.40, 33.90)

cyl, n/N (%)

  

    4

11/32 (34%)

    6

7/32 (22%)

    8

14/32 (44%)

disp

  

    Mean ± SD

230.72 ± 123.94

    Median (Min, Max)

196.30 (71.10, 472.00)

hp

  

    Mean ± SD

146.69 ± 68.56

    Median (Min, Max)

123.00 (52.00, 335.00)

drat

  

    Mean ± SD

3.60 ± 0.53

    Median (Min, Max)

3.70 (2.76, 4.93)

wt

  

    Mean ± SD

3.22 ± 0.98

    Median (Min, Max)

3.33 (1.51, 5.42)

qsec

  

    Mean ± SD

17.85 ± 1.79

    Median (Min, Max)

17.71 (14.50, 22.90)

vs, n/N (%)

  

    V-shaped

18/32 (56%)

    straight

14/32 (44%)

am, n/N (%)

  

    automatic

19/32 (59%)

    manual

13/32 (41%)

gear, n/N (%)

  

    3

15/32 (47%)

    4

12/32 (38%)

    5

5/32 (16%)

carb, n/N (%)

  

    1

7/32 (22%)

    2

10/32 (31%)

    3

3/32 (9.4%)

    4

10/32 (31%)

    6

1/32 (3.1%)

    8

1/32 (3.1%)
