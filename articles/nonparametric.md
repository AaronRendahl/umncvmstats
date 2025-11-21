# Nonparametric Inference

## One Sample Wilcoxon test

``` r
one_wilcoxon_inference(wt ~ 1, data = mtcars2)
```

| response                                                                                         | n   | pseudomedian | conf.low | conf.high | null  | V    | p.value   |
|--------------------------------------------------------------------------------------------------|-----|--------------|----------|-----------|-------|------|-----------|
| wt                                                                                               | 32  | 3.19         | 2.83     | 3.51      | 0.000 | 528  | \< 0.0001 |
| Wilcoxon signed rank test with continuity correction (two.sided), with 95% confidence intervals. |     |              |          |           |       |      |           |
| Approximate p-value used, due to ties.                                                           |     |              |          |           |       |      |           |
| Approximate confidence interval used, due to ties.                                               |     |              |          |           |       |      |           |

### Separately by another categorical variable

``` r
one_wilcoxon_inference(wt ~ am, data = mtcars2)
```

| response                                                                                           | variable       | n   | pseudomedian | conf.low | conf.high | null  | V     | p.value  | footnote  |
|----------------------------------------------------------------------------------------------------|----------------|-----|--------------|----------|-----------|-------|-------|----------|-----------|
| wt                                                                                                 | am = automatic | 19  | 3.63         | 3.44     | 4.28      | 0.000 | 190   |   0.0001 | ^(1,2,3)  |
| wt                                                                                                 | am = manual    | 13  | 2.39         | 2.02     | 2.78      | 0.000 |  91.0 |   0.0002 | ⁴         |
| ¹ Wilcoxon signed rank test with continuity correction (two.sided), with 95% confidence intervals. |                |     |              |          |           |       |       |          |           |
| ² Approximate p-value used, due to ties.                                                           |                |     |              |          |           |       |       |          |           |
| ³ Approximate confidence interval used, due to ties.                                               |                |     |              |          |           |       |       |          |           |
| ⁴ Wilcoxon signed rank exact test (two.sided), with 95% confidence intervals.                      |                |     |              |          |           |       |       |          |           |

## Two Sample Wilcoxon test

``` r
two_wilcoxon_inference(wt ~ am, data = mtcars2)
```

| response                                                                                      | variable               | pseudomedian | conf.low | conf.high | null  | W    | p.value   |
|-----------------------------------------------------------------------------------------------|------------------------|--------------|----------|-----------|-------|------|-----------|
| wt                                                                                            | am: automatic - manual | 1.28         | 0.79     | 1.82      | 0.000 | 230  | \< 0.0001 |
| Wilcoxon rank sum test with continuity correction (two.sided), with 95% confidence intervals. |                        |              |          |           |       |      |           |
| Approximate p-value used, due to ties.                                                        |                        |              |          |           |       |      |           |
| Approximate confidence interval used, due to ties.                                            |                        |              |          |           |       |      |           |

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

| response                                                                                                                                              | variable   | pseudomedian | conf.low | conf.high | null  | W    | p.value   | p.adjust |
|-------------------------------------------------------------------------------------------------------------------------------------------------------|------------|--------------|----------|-----------|-------|------|-----------|----------|
| wt                                                                                                                                                    | cyl: 4 - 6 | −0.93        | −1.52    | −0.16     | 0.000 | 8.00 |   0.0066  |   0.020  |
| wt                                                                                                                                                    | cyl: 4 - 8 | −1.62        | −2.27    | −0.97     | 0.000 | 1.00 | \< 0.0001 |   0.0001 |
| wt                                                                                                                                                    | cyl: 6 - 8 | −0.66        | −1.90    | −0.13     | 0.000 | 9.00 |   0.0032  |   0.0095 |
| Wilcoxon rank sum test with continuity correction (two.sided), with 95% confidence intervals, adjusted for 3 comparisons using the Bonferroni method. |            |              |          |           |       |      |           |          |
| Approximate p-value used, due to ties.                                                                                                                |            |              |          |           |       |      |           |          |
| Approximate confidence interval used, due to ties.                                                                                                    |            |              |          |           |       |      |           |          |
| p-values adjusted for 3 multiple comparisons using the Bonferroni method.                                                                             |            |              |          |           |       |      |           |          |

## Paired Wilcoxon test

``` r
paired_wilcoxon_inference(score2 - score1 ~ 1, data = passfail)
```

| response                                                                                         | pseudomedian | conf.low | conf.high | null  | V    | p.value |
|--------------------------------------------------------------------------------------------------|--------------|----------|-----------|-------|------|---------|
| score2 - score1                                                                                  | 3.1          | 0.5      | 5.5       | 0.000 | 881  |   0.019 |
| Wilcoxon signed rank test with continuity correction (two.sided), with 95% confidence intervals. |              |          |           |       |      |         |
