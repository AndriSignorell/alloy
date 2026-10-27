# Prediction Errors of Fitted Regression Models

Methods of the prediction error measures of DescToolsX for fitted
models: the mean absolute error
([`DescToolsX::mae()`](https://andrisignorell.github.io/DescToolsX/reference/mae.html)),
the mean squared error
([`DescToolsX::mse()`](https://andrisignorell.github.io/DescToolsX/reference/mse.html))
and its root
([`DescToolsX::rmse()`](https://andrisignorell.github.io/DescToolsX/reference/rmse.html)),
and the mean absolute percentage error, plain
([`DescToolsX::mape()`](https://andrisignorell.github.io/DescToolsX/reference/mape.html))
and symmetric
([`DescToolsX::smape()`](https://andrisignorell.github.io/DescToolsX/reference/smape.html)).
The measures themselves work on predictions and observed values; the
methods here take both from a model.

## Usage

``` r
# S3 method for class 'FitMod'
mae(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
mse(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
rmse(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
mape(x, newdata = NULL, ...)

# S3 method for class 'FitMod'
smape(x, newdata = NULL, ...)

# S3 method for class 'lm'
mae(x, ...)

# S3 method for class 'lm'
mse(x, ...)

# S3 method for class 'lm'
rmse(x, ...)

# S3 method for class 'lm'
mape(x, ...)

# S3 method for class 'lm'
smape(x, ...)
```

## Arguments

- x:

  a regression model fitted with
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
  or an `lm` (incl. `glm`) object

- newdata:

  an optional data frame to evaluate, containing the response. `NULL`
  (default) uses the training data. For models fitted with
  [`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
  only.

- ...:

  further arguments passed to the measure, e.g. `na.rm`

## Value

As the respective measure in DescToolsX.

## Details

For a model fitted with
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md)
the predictions come from
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md)
on the response scale, so the methods work alike for every regression
engine (linear, generalised linear, robust, trees, random forests, ...),
on the training data or on `newdata`.

For an `lm` or `glm` object the fitted values on the response scale and
the stored response are used. The response must be a numeric vector: a
logistic `glm` with a factor or two-column response is refused.

Measures on the training data are optimistic;
[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
reports the cross-validated root mean squared and mean absolute error.

## See also

[`crossVal()`](https://andrisignorell.github.io/alloy/reference/crossVal.md)
for cross-validated measures,
[classification-metrics](https://andrisignorell.github.io/alloy/reference/classification-metrics.md)

Other modelling:
[`classification-metrics`](https://andrisignorell.github.io/alloy/reference/classification-metrics.md),
[`fitMod()`](https://andrisignorell.github.io/alloy/reference/fitMod.md),
[`logitFirth`](https://andrisignorell.github.io/alloy/reference/logitFirth.md),
[`plot.FitMod()`](https://andrisignorell.github.io/alloy/reference/plot.FitMod.md),
[`predict.FitMod()`](https://andrisignorell.github.io/alloy/reference/predict.FitMod.md),
[`predictors()`](https://andrisignorell.github.io/alloy/reference/predictors.md),
[`print.FitMod()`](https://andrisignorell.github.io/alloy/reference/print.FitMod.md)

## Examples

``` r
fit <- fitMod(mpg ~ hp + wt, mtcars, engine = "lm")
mae(fit)
#> [1] 1.901484
rmse(fit)
#> [1] 2.468854
mape(fit)
#> [1] 0.09742983

# a plain lm
rmse(lm(mpg ~ hp + wt, mtcars))
#> [1] 2.468854

# out of sample
set.seed(1)
d <- splitTrainTest(mtcars, p = 0.3)
rmse(update(fit, data = d$train), newdata = d$test)
#> [1] 2.820497
```
