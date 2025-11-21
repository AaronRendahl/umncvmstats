# Display standard diagnostic plots for a model

Display standard diagnostic plots for a model.

## Usage

``` r
diag_plots(model)
```

## Arguments

- model:

  the model to display diagnostic plots for.

## Details

The four standard diagnostic plots created by `plot.lm` are shown, but
using `ggplot` graphics techniques. These plots include:

1.  Residual vs Fitted

2.  Normal Q-Q

3.  Scale-Location

4.  Residuals vs Leverage
