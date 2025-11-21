# Kruskal-Wallis test

Perform the Kruskal Wallis test to compare two or more groups

## Usage

``` r
kruskal_wallis_test(formula, data)
```

## Arguments

- formula:

  a formula of the form `y ~ x`, where `y` is a numeric variable and `x`
  is a factor variable. To perform test within subgroups, use
  `y ~ x | g`, where `x` and `g` are factor variables.

- data:

  a data frame containing the values in the formula.
