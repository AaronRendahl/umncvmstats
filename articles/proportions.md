# Inference for Proportions

## Inference for One Proportion

It will automatically choose between Wilson’s CI and the “exact” CI. No
null hypothesis is tested by default.

``` r
one_proportion_inference(vs ~ 1, data = mtcars2)
```

| response                                                             | x   | n   | proportion | SE    | conf.low | conf.high |
|----------------------------------------------------------------------|-----|-----|------------|-------|----------|-----------|
| vs = straight                                                        | 14  | 32  | 0.438      | 0.088 | 0.282    | 0.607     |
| Wilson's proportion test (two.sided), with 95% confidence intervals. |     |     |            |       |          |           |

### Separately by another categorical variable

``` r
one_proportion_inference(vs ~ am, data = mtcars2)
```

| response                                                             | variable       | x   | n   | proportion | SE   | conf.low | conf.high |
|----------------------------------------------------------------------|----------------|-----|-----|------------|------|----------|-----------|
| vs = straight                                                        | am = automatic | 7   | 19  | 0.37       | 0.11 | 0.19     | 0.59      |
| vs = straight                                                        | am = manual    | 7   | 13  | 0.54       | 0.14 | 0.29     | 0.77      |
| Wilson's proportion test (two.sided), with 95% confidence intervals. |                |     |     |            |      |          |           |

## Inference for Two Proportions

It will automatically choose between the asymptotic test (with or
without continuity correction) and Fisher’s test.

``` r
two_proportion_inference(vs ~ am, data = mtcars2)
```

| response                                                                                                         | variable               | difference | SE   | conf.low | conf.high | chisq.value | p.value |
|------------------------------------------------------------------------------------------------------------------|------------------------|------------|------|----------|-----------|-------------|---------|
| vs = straight                                                                                                    | am: automatic - manual | −0.17      | 0.18 | −0.58    | 0.24      | 0.348       |   0.56  |
| 2-sample test for equality of proportions with continuity correction (two.sided), with 95% confidence intervals. |                        |            |      |          |           |             |         |

### together in one table

``` r
combine_tests(
  one_proportion_inference(vs ~ am, data = mtcars2),
  two_proportion_inference(vs ~ am, data = mtcars2))
```

[TABLE]

## Pairwise Proportion Tests

``` r
combine_tests(
  one_proportion_inference(vs ~ cyl, data = mtcars2),
  pairwise_proportion_inference(vs ~ cyl, data = mtcars2))
```

[TABLE]

## Paired Proportion Test (McNemar’s)

``` r
combine_tests(
  one_proportion_inference(pass1 + pass2 ~ 1, data = passfail),
  one_proportion_inference(pass2 ~ pass1, data = passfail, all_success = TRUE),
  paired_proportion_inference(pass2 - pass1 ~ 1, data = passfail))
```

[TABLE]

## Independence Test

It will automatically choose between the chi-squared test and Fisher’s
test.

``` r
combine_tests(
  one_proportion_inference(cyl ~ vs, data = mtcars2, all_success = TRUE),
  independence_test(cyl ~ vs, data = mtcars2))
```

[TABLE]
