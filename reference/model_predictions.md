# Get model predictions

Calculate predictions (fitted values) and prediction intervals for a
linear model.

## Usage

``` r
model_predictions(
  model,
  at,
  newdata,
  level = 0.95,
  backtransform = TRUE,
  se_fit = FALSE
)
```

## Arguments

- model:

  a linear model

- at:

  the values for each predictor to predict at; uses all combinations of
  specified values

- newdata:

  alternative to at, to specify specific combinations of predictors to
  predict at

- level:

  desired prediction level

- backtransform:

  whether or not to backtransform predictions from models on the log
  scale

- se_fit:

  whether or not to report the standard error of the predicted value
