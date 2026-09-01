# Nonparametric Inference

## One Sample Wilcoxon test

``` r

one_wilcoxon_inference(wt ~ 1, data = mtcars2)
```

| response | n | median | pseudomedian | conf.low | conf.high | null | V | p.value |
|----|----|----|----|----|----|----|----|----|
| wt | 32 | 3.33 | 3.19 | 2.83 | 3.51 | 0.000 | 528  | \< 0.0001 |
| Wilcoxon signed rank exact test (two.sided), with 95% confidence intervals. |  |  |  |  |  |  |  |  |

### Separately by another categorical variable

``` r

one_wilcoxon_inference(wt ~ am, data = mtcars2)
```

| response | variable | n | median | pseudomedian | conf.low | conf.high | null | V | p.value |
|----|----|----|----|----|----|----|----|----|----|
| wt | am = automatic | 19 | 3.52 | 3.63 | 3.44 | 4.27 | 0.000 | 190   | \< 0.0001 |
| wt | am = manual | 13 | 2.32 | 2.39 | 2.02 | 2.78 | 0.000 |  91.0 |   0.0002 |
| Wilcoxon signed rank exact test (two.sided), with 95% confidence intervals. |  |  |  |  |  |  |  |  |  |

## Two Sample Wilcoxon test

``` r

two_wilcoxon_inference(wt ~ am, data = mtcars2)
```

| response | variable | pseudomedian | conf.low | conf.high | null | W | p.value |
|----|----|----|----|----|----|----|----|
| wt | am: automatic - manual | 1.28 | 0.80 | 1.79 | 0.000 | 230  | \< 0.0001 |
| Wilcoxon rank sum exact test (two.sided), with 95% confidence intervals. |  |  |  |  |  |  |  |

## Kruskal-Wallis test

``` r

kruskal_wallis_test(wt ~ am, data=mtcars2)
```

| response                     | variable | df  | chisq | p.value   |
|------------------------------|----------|-----|-------|-----------|
| wt                           | am       | 1   | 16.9  | \< 0.0001 |
| Kruskal-Wallis rank sum test |          |     |       |           |

## Pairwise Wilcoxon tests

``` r

pairwise_wilcoxon_inference(wt ~ cyl, data = mtcars2)
```

| response | variable | pseudomedian | conf.low | conf.high | null | W | p.value | p.adjust |
|----|----|----|----|----|----|----|----|----|
| wt | cyl: 4 - 6 | −0.94 | −1.50 | −0.25 | 0.000 | 8.00 |   0.0040 |   0.012 |
| wt | cyl: 4 - 8 | −1.62 | −2.24 | −0.98 | 0.000 | 1.00 | \< 0.0001 | \< 0.0001 |
| wt | cyl: 6 - 8 | −0.66 | −1.88 | −0.13 | 0.000 | 9.00 |   0.0015 |   0.0045 |
| Wilcoxon rank sum exact test (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |  |  |  |  |  |  |  |  |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method. |  |  |  |  |  |  |  |  |

## Paired Wilcoxon test

``` r

paired_wilcoxon_inference(score2 - score1 ~ 1, data = passfail)
```

| response | pseudomedian | conf.low | conf.high | null | V | p.value |
|----|----|----|----|----|----|----|
| score2 - score1 | 3.1 | 0.5 | 5.5 | 0.000 | 881  |   0.019 |
| Wilcoxon signed rank test with continuity correction (two.sided), with 95% confidence intervals. |  |  |  |  |  |  |
