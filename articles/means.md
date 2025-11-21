# Inference for Means

## One Sample t-test

``` r
one_t_inference(wt ~ 1, data = mtcars2)
```

| response                                                      | n   | mean | SE   | df  | conf.low | conf.high |
|---------------------------------------------------------------|-----|------|------|-----|----------|-----------|
| wt                                                            | 32  | 3.22 | 0.17 | 31  | 2.86     | 3.57      |
| One Sample t-test (two.sided), with 95% confidence intervals. |     |      |      |     |          |           |

### Separately by another categorical variable

``` r
one_t_inference(wt ~ am, data = mtcars2)
```

| response                                                      | variable       | n   | mean | SE   | df  | conf.low | conf.high |
|---------------------------------------------------------------|----------------|-----|------|------|-----|----------|-----------|
| wt                                                            | am = automatic | 19  | 3.77 | 0.18 | 18  | 3.39     | 4.14      |
| wt                                                            | am = manual    | 13  | 2.41 | 0.17 | 12  | 2.04     | 2.78      |
| One Sample t-test (two.sided), with 95% confidence intervals. |                |     |      |      |     |          |           |

## Two Sample t-test

``` r
two_t_inference(wt ~ am, data = mtcars2)
```

| response                                                            | variable               | difference | SE   | df   | conf.low | conf.high | null  | t.value | p.value   |
|---------------------------------------------------------------------|------------------------|------------|------|------|----------|-----------|-------|---------|-----------|
| wt                                                                  | am: automatic - manual | 1.36       | 0.25 | 29.2 | 0.85     | 1.86      | 0.000 | 5.49    | \< 0.0001 |
| Welch Two Sample t-test (two.sided), with 95% confidence intervals. |                        |            |      |      |          |           |       |         |           |

### together in one table

``` r
combine_tests(
  one_t_inference(wt ~ am, data = mtcars2),
  two_t_inference(wt ~ am, data = mtcars2))
```

[TABLE]

## Pairwise t-tests

``` r
combine_tests(
  one_t_inference(wt ~ cyl, data = mtcars2),
  pairwise_t_inference(wt ~ cyl, data = mtcars2))
```

[TABLE]

## Paired t-test

``` r
combine_tests(
  one_t_inference(score1 + score2 ~ 1, data = passfail),
  paired_t_inference(score2 - score1 ~ 1, data = passfail))
```

[TABLE]

## Log transformations

By default, responses using `log(...)` are back-transformed. To keep the
result on the log scale, use `backtransform = FALSE`.

``` r
combine_tests(
  one_t_inference(log(wt) ~ am, data = mtcars2, backtransform = FALSE),
  two_t_inference(log(wt) ~ am, data = mtcars2, backtransform = FALSE),
  one_t_inference(log(wt) ~ am, data = mtcars2),
  two_t_inference(log(wt) ~ am, data = mtcars2))
```

[TABLE]

``` r

combine_tests(
  one_t_inference(log(score1) + log(score2) ~ 1, data = passfail),
  paired_t_inference(log(score2) - log(score1) ~ 1, data = passfail))
```

[TABLE]
