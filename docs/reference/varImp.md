# Variable importance for machine learning models

Extracts and normalises variable importance scores from fitted machine
learning models. Returns a `data.frame` sorted by importance, scaled to
a 0–100 range.

## Usage

``` r
varImp(x, scale = c("max", "sum", "none"), sort = TRUE, ...)
```

## Arguments

- x:

  A fitted model of class `"FitMod"`. Supported fitting functions:
  `"rpart"`, `"C5.0"`, `"randomForest"`, `"nnet"`, `"glmnet"`,
  `"xgboost"`.

- scale:

  Character string controlling the scaling of importance scores. One of
  `"max"` (default: best predictor = 100, others relative), `"sum"`
  (scores sum to 100, interpretable as percentage share), or `"none"`
  (raw scores as returned by the underlying method).

- sort:

  Logical. If `TRUE` (default), rows are sorted in descending order of
  importance.

- ...:

  Further arguments passed to the underlying importance method.

## Value

A `data.frame` of class `c("varImp", "data.frame")` with columns:

- variable:

  Character: predictor name.

- importance:

  Numeric: importance score (0–100 if `scale = TRUE`).

## See also

[`plot.varImp`](plot.varImp.md)

Other regression.utils: [`coefCI()`](coefCI.md),
[`pseudoR2()`](pseudoR2.md), [`rSq()`](rSq.md),
[`refLevel()`](refLevel.md), [`response()`](response.md),
[`vif()`](vif.md)

## Examples

``` r
fitRf <- fitMod(ice_cream ~ video + puzzle + female,
                IceCream, fitfn = "randomForest")
vi <- varImp(fitRf)
vi
#>   variable importance
#> 1    video  100.00000
#> 2   puzzle   81.01213
#> 3   female   21.25547
plot(vi)

```
